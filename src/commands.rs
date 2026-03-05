use anyhow::{bail, Context, Result};
use chrono::NaiveDate;
use git2::Repository;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::{debug, info, warn};

use crate::cli::{JournalArgs, MirrorCommand, PruneArgs, WebCommand};
use crate::markdown::{self, parse_frontmatter};
use crate::{git, github, mirror, picker, sparse, web, PROJECTS_DIR};

/// Call the running proxy's management API. Returns None if no proxy is running.
async fn proxy_api_get(port: u16, path: &str) -> Option<reqwest::Response> {
    let url = format!("http://127.0.0.1:{}/api/v1/{}", port, path);
    reqwest::get(&url).await.ok()
}

async fn proxy_api_post(
    port: u16,
    path: &str,
    body: serde_json::Value,
) -> Option<reqwest::Response> {
    let client = reqwest::Client::new();
    client
        .post(format!("http://127.0.0.1:{}/api/v1/{}", port, path))
        .json(&body)
        .send()
        .await
        .ok()
}

pub fn validate_project_name(name: &str) -> Result<()> {
    if name.is_empty() {
        bail!("Project name cannot be empty");
    }
    if name.contains('/') || name.contains('\\') || name.contains("..") {
        bail!("Project name cannot contain path separators or '..'");
    }
    if name.starts_with('.') || name.starts_with('-') {
        bail!("Project name cannot start with '.' or '-'");
    }
    Ok(())
}

fn format_date(date: Option<NaiveDate>) -> String {
    date.map(|d| d.format("%Y-%m-%d").to_string())
        .unwrap_or_else(|| "-".to_string())
}

#[derive(Debug)]
pub struct ProjectInfo {
    pub name: String,
    pub description: String,
    pub created: Option<NaiveDate>,
    pub updated: Option<NaiveDate>,
    pub focused: bool,
}

impl ProjectInfo {
    /// Fallback when git metadata is unavailable.
    pub fn name_only(name: String) -> Self {
        Self {
            name,
            description: String::new(),
            created: None,
            updated: None,
            focused: false,
        }
    }
}

pub fn gather_project_info(
    repo: &Repository,
    root: &Path,
    projects: &[String],
    focused: &[String],
) -> Result<Vec<ProjectInfo>> {
    projects
        .iter()
        .map(|name| {
            let fm = markdown::load_readme_frontmatter(repo, root, name);

            Ok(ProjectInfo {
                name: name.clone(),
                description: fm
                    .as_ref()
                    .and_then(|f| f.description.clone())
                    .unwrap_or_default(),
                created: fm.as_ref().and_then(|f| f.created),
                updated: git::get_project_updated(repo, name)?,
                focused: focused.contains(name),
            })
        })
        .collect()
}

fn render_project_table(projects: &[ProjectInfo], show_marker: bool) -> String {
    let mut md = String::new();

    if show_marker {
        md.push_str("| | Project | Description | Created | Updated |\n");
        md.push_str("|:-|:--------|:------------|:--------|:--------|\n");
    } else {
        md.push_str("| Project | Description | Created | Updated |\n");
        md.push_str("|:--------|:------------|:--------|:--------|\n");
    }

    for p in projects {
        if show_marker {
            let marker = if p.focused { "**→**" } else { "" };
            md.push_str(&format!(
                "| {} | {} | {} | {} | {} |\n",
                marker,
                p.name,
                p.description,
                format_date(p.created),
                format_date(p.updated)
            ));
        } else {
            md.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                p.name,
                p.description,
                format_date(p.created),
                format_date(p.updated)
            ));
        }
    }

    md
}

pub fn cmd_list(repo: &Repository, root: &Path, all: bool, use_color: bool) -> Result<()> {
    let projects = if all {
        git::list_all_projects(repo)?
    } else {
        sparse::get_focused_projects(root)?
    };

    if projects.is_empty() {
        if all {
            info!("No projects found in projects/");
        } else {
            info!("No projects currently focused");
        }
        return Ok(());
    }

    let focused = if all {
        sparse::get_focused_projects(root).unwrap_or_default()
    } else {
        Vec::new()
    };

    let infos = gather_project_info(repo, root, &projects, &focused)?;
    let table = render_project_table(&infos, all);
    markdown::skin(use_color).print_text(&table);
    Ok(())
}

pub fn cmd_add(root: &Path, project: &str, create: bool) -> Result<PathBuf> {
    validate_project_name(project)?;
    if create {
        create_project(root, project)?;
    }
    sparse::add_project(root, project)?;
    info!("Added {} to focus", project);
    Ok(root.join(PROJECTS_DIR).join(project))
}

pub fn cmd_rm(repo: &Repository, root: &Path, project: &str) -> Result<()> {
    validate_project_name(project)?;

    let focused = sparse::get_focused_projects(root)?;
    if focused.is_empty() {
        warn!("No projects currently focused");
        return Ok(());
    }

    if !focused.contains(&project.to_string()) {
        let all_projects = git::list_all_projects(repo)?;
        if all_projects.contains(&project.to_string()) {
            warn!("Project '{}' is not focused", project);
        } else {
            // Suggest similar focused projects
            let suggestions = picker::fuzzy_match(&focused, project);
            if suggestions.is_empty() {
                bail!("Project '{}' does not exist", project);
            } else {
                let hint = suggestions
                    .into_iter()
                    .take(3)
                    .collect::<Vec<_>>()
                    .join(" ");
                bail!(
                    "Project '{}' does not exist. Did you mean: {}",
                    project,
                    hint
                );
            }
        }
        return Ok(());
    }

    sparse::drop_project(root, project)?;
    info!("Removed {} from focus", project);
    Ok(())
}

pub fn cmd_cd(
    repo: &Repository,
    root: &Path,
    query: Option<String>,
    use_color: bool,
) -> Result<Option<PathBuf>> {
    let projects = git::list_all_projects(repo)?;
    if projects.is_empty() {
        bail!("No projects found");
    }

    let selected = picker::fuzzy_pick(projects, query.as_deref(), use_color)?;

    if let Some(ref project) = selected {
        sparse::add_project(root, project)?;
    }

    Ok(selected.map(|p| root.join(PROJECTS_DIR).join(p)))
}

pub fn cmd_init(shell: &str) -> Result<String> {
    if shell != "fish" {
        bail!("Unsupported shell: {}. Use fish.", shell);
    }

    let bin = std::env::current_exe()
        .context("Failed to get current executable path")?
        .display()
        .to_string();

    Ok(format!(
        r#"function meow
  set -l __meow_bin "{bin}"
  switch $argv[1]
    case list ls rm drop init journal zellij z prune pull mirror web --help -h --version -V
      $__meow_bin $argv
    case add cd
      set -l result ($__meow_bin $argv)
      and test -n "$result"
      and cd "$result"
    case '*'
      set -l result ($__meow_bin cd $argv)
      and test -n "$result"
      and cd "$result"
  end
end

function m
  meow $argv
end

function mz
  meow zellij $argv
end

function mw
  meow web $argv
end"#
    ))
}

pub fn cmd_pull(root: &Path) -> Result<()> {
    let status = Command::new("git")
        .args(["-C", &root.display().to_string(), "pull"])
        .status()
        .context("Failed to run git pull")?;

    if !status.success() {
        bail!("git pull failed");
    }
    Ok(())
}

pub fn cmd_prune(root: &Path, args: PruneArgs) -> Result<()> {
    use std::io::{self, Write};

    let trees_dir = git::worktree_dir()?;

    if !trees_dir.exists() {
        info!("No worktrees directory found");
        git::prune_worktrees(root)?;
        return Ok(());
    }

    let entries: Vec<_> = fs::read_dir(&trees_dir)?
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().map(|ft| ft.is_dir()).unwrap_or(false))
        .collect();

    if entries.is_empty() {
        info!("No worktrees to prune");
        git::prune_worktrees(root)?;
        return Ok(());
    }

    let mut removed = 0;
    let mut skipped = 0;

    for entry in entries {
        let path = entry.path();
        let name = path.file_name().unwrap_or_default().to_string_lossy();

        let is_dirty = git::is_worktree_dirty(&path);

        if is_dirty && !args.force {
            #[allow(clippy::print_stdout)]
            {
                print!("{} has uncommitted changes. Delete? [y/N] ", name);
                io::stdout().flush()?;
            }

            let mut input = String::new();
            let _ = io::stdin().read_line(&mut input)?;

            if !input.trim().eq_ignore_ascii_case("y") {
                info!("Skipping {}", name);
                skipped += 1;
                continue;
            }
        }

        debug!("Removing worktree: {}", path.display());
        fs::remove_dir_all(&path)
            .with_context(|| format!("Failed to remove {}", path.display()))?;
        removed += 1;
    }

    // Clean up git's worktree metadata
    git::prune_worktrees(root)?;

    #[allow(clippy::print_stdout)]
    {
        if removed > 0 || skipped > 0 {
            println!(
                "Pruned {} worktree(s){}",
                removed,
                if skipped > 0 {
                    format!(", skipped {}", skipped)
                } else {
                    String::new()
                }
            );
        } else {
            println!("No worktrees to prune");
        }
    }

    Ok(())
}

pub fn cmd_journal(
    repo: &git2::Repository,
    root: &Path,
    args: JournalArgs,
    use_color: bool,
) -> Result<()> {
    let (since, until) = if let Some(days) = args.days {
        let today = chrono::Local::now().date_naive();
        let since = today - chrono::Duration::days(i64::from(days));
        (Some(since), Some(today))
    } else {
        (args.since, args.until)
    };

    let projects: Vec<String> = if args.all {
        sparse::get_focused_projects(root)?
    } else {
        let project = match args.project {
            Some(p) => p,
            None => git::detect_project_from_cwd(root)?
                .context("Not in a project directory. Specify project name.")?,
        };
        vec![project]
    };

    if projects.is_empty() {
        info!("No active projects");
        return Ok(());
    }

    let mut found_any = false;
    for project in &projects {
        let entries = markdown::read_journal(root, project, since, until)?;

        if args.git {
            // Fetch commits and interleave with journal entries
            let commits = git::get_project_commits(repo, project, since, until)?;
            let timeline = build_timeline(entries, commits);

            if timeline.is_empty() {
                continue;
            }
            if found_any {
                markdown::skin(use_color).print_text("\n---\n");
            }
            found_any = true;
            let md = markdown::render_timeline(project, &timeline);
            markdown::skin(use_color).print_text(&md);
        } else {
            // Original behavior: just journal entries
            if entries.is_empty() {
                continue;
            }
            if found_any {
                markdown::skin(use_color).print_text("\n---\n");
            }
            found_any = true;
            let md = markdown::render_entries(project, &entries);
            markdown::skin(use_color).print_text(&md);
        }
    }

    if !found_any {
        info!("No journal entries found");
    }

    Ok(())
}

/// Build a timeline from journal entries and commits, sorted newest first.
fn build_timeline(
    entries: Vec<markdown::JournalEntry>,
    commits: Vec<markdown::CommitEntry>,
) -> Vec<markdown::TimelineItem> {
    use std::cmp::Reverse;

    let mut timeline: Vec<markdown::TimelineItem> = entries
        .into_iter()
        .map(markdown::TimelineItem::Journal)
        .chain(commits.into_iter().map(markdown::TimelineItem::Commit))
        .collect();

    timeline.sort_by_key(|item| Reverse(item.datetime()));
    timeline
}

fn create_project(root: &Path, project: &str) -> Result<()> {
    let project_dir = root.join(PROJECTS_DIR).join(project);

    if project_dir.exists() {
        bail!(
            "Project directory already exists: {}",
            project_dir.display()
        );
    }

    fs::create_dir_all(&project_dir).context("Failed to create project directory")?;

    let today = chrono::Local::now().format("%Y-%m-%d");
    let readme_content = format!(
        r#"---
created: {today}
description: TODO Add description
---
# {project}
"#
    );
    fs::write(project_dir.join("README.md"), readme_content)
        .context("Failed to create README.md")?;

    let agents_content = format!("# {project}\n");
    fs::write(project_dir.join("AGENTS.md"), agents_content)
        .context("Failed to create AGENTS.md")?;

    let shell_nix_content = r#"{pkgs, ...}:
pkgs.mkShell {
  buildInputs = [
  ];
}
"#;
    fs::write(project_dir.join("shell.nix"), shell_nix_content)
        .context("Failed to create shell.nix")?;

    let envrc_content = format!("use flake ../..#{project}\n");
    fs::write(project_dir.join(".envrc"), envrc_content).context("Failed to create .envrc")?;

    info!("Created project {}", project);
    Ok(())
}

/// Inject tab name and cwd into a zellij layout.
/// Wraps the implicit default tab in an explicit `tab name="..." cwd="..." { }` block
/// and adds name to tabs in swap layouts.
fn inject_tab_name(layout: &str, tab_name: &str, cwd: &Path) -> String {
    let safe_name = tab_name.replace('"', r#"\""#);
    let safe_cwd = cwd.display().to_string().replace('"', r#"\""#);

    // Wrap implicit default tab with explicit tab block
    let with_tab = layout.replace(
        "layout {",
        &format!(
            "layout {{\n    tab name=\"{}\" cwd=\"{}\" {{",
            safe_name, safe_cwd
        ),
    );

    // Close the tab wrapper before swap layouts or end
    let closed = if with_tab.contains("swap_tiled_layout") {
        with_tab.replacen("swap_tiled_layout", "}\n\n    swap_tiled_layout", 1)
    } else {
        with_tab
            .rsplit_once('}')
            .map(|(before, _)| format!("{}    }}\n}}", before))
            .unwrap_or(with_tab)
    };

    // Add name to unnamed tabs in swap layouts
    closed
        .replace("\ttab {", &format!("\ttab name=\"{}\" {{", safe_name))
        .replace(
            "        tab {",
            &format!("        tab name=\"{}\" {{", safe_name),
        )
}

/// Resolve the layout to use, injecting tab name where possible.
/// Priority: project .meow.d/layout.kdl > ~/.config/zellij/layouts/{name}.kdl > named layout
fn resolve_layout(project_path: &Path, tab_name: &str, layout_name: &str) -> Option<String> {
    // Project-specific layout with template substitution
    let custom = project_path.join(".meow.d/layout.kdl");
    if custom.exists() {
        return fs::read_to_string(&custom).ok().map(|template| {
            template
                .replace("{{PROJECT_PATH}}", &project_path.display().to_string())
                .replace("{{TAB_NAME}}", tab_name)
        });
    }

    // Try to read base layout from zellij config and inject tab name
    // Zellij uses ~/.config/zellij even on macOS (not ~/Library/Application Support)
    let layout_path = dirs::home_dir()?
        .join(".config/zellij/layouts")
        .join(format!("{}.kdl", layout_name));
    if layout_path.exists() {
        return fs::read_to_string(&layout_path)
            .ok()
            .map(|content| inject_tab_name(&content, tab_name, project_path));
    }

    None // Fall back to named layout (no tab name injection possible)
}

#[allow(clippy::too_many_arguments)]
pub fn cmd_zellij(
    root: &Path,
    query: Option<String>,
    branch_parts: Vec<String>,
    layout: &str,
    worktree: bool,
    prompt: Option<String>,
    command: Option<String>,
    use_color: bool,
) -> Result<()> {
    if prompt.is_some() && command.is_some() {
        bail!("Cannot specify both --prompt and --command");
    }

    let focused = sparse::get_focused_projects(root)?;
    if focused.is_empty() {
        bail!("No projects currently focused");
    }

    let project =
        picker::fuzzy_pick(focused, query.as_deref(), use_color)?.context("No project selected")?;

    let exec = prompt.as_ref().or(command.as_ref());

    // Determine worktree mode:
    // - branch_parts non-empty → worktree with kebab-cased branch name
    // - worktree flag → worktree with timestamp
    // - neither → normal project path
    let use_worktree = !branch_parts.is_empty() || worktree;
    let branch_name = if !branch_parts.is_empty() {
        Some(branch_parts.join("-"))
    } else {
        None
    };

    let (project_path, tab_name) = if use_worktree {
        let wt_path = git::create_worktree(root, &project, branch_name.as_deref())?;
        // Extract worktree name from path for tab name
        // Path is ~/.meow/trees/{branch|project-timestamp}/projects/{project}
        let wt_name = wt_path
            .parent() // projects/
            .and_then(|p| p.parent()) // {project}-{branch}/
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
            .unwrap_or(&project)
            .to_string();
        (wt_path, wt_name)
    } else {
        (root.join(PROJECTS_DIR).join(&project), project.clone())
    };

    // If --prompt or --command was given, start opencode serve, create a session,
    // fire the prompt/command, and build a layout that attaches the TUI to that session.
    let exec_layout: Option<String> = if exec.is_some() {
        if !use_worktree {
            bail!(
                "--prompt/--command requires a worktree (provide branch name parts or --worktree)"
            );
        }

        // Spawn opencode serve with output going to a log file (no pipes).
        // Pipes cause broken-pipe spin loops when meow exits.
        let log_dir = crate::config::data_dir().context("No data dir")?;
        let _ = fs::create_dir_all(&log_dir);
        let log_path = log_dir.join(format!("serve-{}.log", &tab_name));
        let log_stdout = fs::File::create(&log_path).context("Failed to create serve log file")?;
        let log_stderr = log_stdout
            .try_clone()
            .context("Failed to clone log file handle")?;

        let child = Command::new("opencode")
            .args([
                "serve",
                "--port",
                "0",
                "--hostname",
                "127.0.0.1",
                "--print-logs",
            ])
            .current_dir(&project_path)
            .stdout(log_stdout)
            .stderr(log_stderr)
            .spawn()
            .context("Failed to spawn opencode serve")?;

        let pid = child.id();

        // Poll the log file for the port announcement (up to 30s).
        let port = {
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(30);
            loop {
                if std::time::Instant::now() > deadline {
                    bail!("Timed out waiting for opencode serve to announce port");
                }
                if let Ok(content) = fs::read_to_string(&log_path) {
                    if let Some(port) = content.lines().find_map(web::extract_port) {
                        break port;
                    }
                }
                std::thread::sleep(std::time::Duration::from_millis(100));
            }
        };

        info!(port, "opencode serve started");

        // HTTP calls (create session, send prompt) via tokio + reqwest.
        let runtime = tokio::runtime::Runtime::new().context("Failed to create async runtime")?;

        let layout_content = runtime.block_on(async {
            let base = format!("http://127.0.0.1:{port}");
            let client = reqwest::Client::new();

            // Poll until the server is ready (port binds before plugins finish loading).
            let deadline = tokio::time::Instant::now() + std::time::Duration::from_secs(30);
            loop {
                if tokio::time::Instant::now() > deadline {
                    bail!("Timed out waiting for opencode serve to be ready");
                }
                if let Ok(r) = client.get(format!("{base}/session")).send().await {
                    if r.status().is_success() {
                        break;
                    }
                }
                tokio::time::sleep(std::time::Duration::from_millis(250)).await;
            }

            info!(port, "opencode serve ready");

            // Create a new session
            let session: serde_json::Value = client
                .post(format!("{base}/session"))
                .json(&serde_json::json!({}))
                .send()
                .await
                .context("Failed to create opencode session")?
                .json()
                .await
                .context("Failed to parse session response")?;

            let sid = session["id"]
                .as_str()
                .context("No session ID in response")?
                .to_owned();

            info!(session_id = %sid, "opencode session created");

            // Fire the prompt or command
            if let Some(ref cmd) = command {
                // Slash command: POST /session/{sid}/command (synchronous endpoint —
                // server doesn't send response headers until the command finishes).
                // Spawn as a background task so we don't block.
                let cmd_str = cmd.strip_prefix('/').unwrap_or(cmd);
                let (cmd_name, cmd_args) = match cmd_str.split_once(char::is_whitespace) {
                    Some((name, args)) => (name.to_owned(), args.to_owned()),
                    None => (cmd_str.to_owned(), String::new()),
                };
                let url = format!("{base}/session/{sid}/command");
                let body = serde_json::json!({
                    "command": &cmd_name,
                    "arguments": &cmd_args
                });
                drop(tokio::spawn(async move {
                    let _ = client.post(url).json(&body).send().await;
                }));
                info!(command = %cmd_name, arguments = %cmd_args, "command dispatched");
            } else if let Some(ref prompt_text) = prompt {
                // Free-form prompt: POST /session/{sid}/prompt_async (returns 204)
                let _ = client
                    .post(format!("{base}/session/{sid}/prompt_async"))
                    .json(&serde_json::json!({
                        "parts": [{"type": "text", "text": prompt_text}]
                    }))
                    .send()
                    .await
                    .context("Failed to send prompt")?;
                info!(%prompt_text, "prompt dispatched");
            }

            // Load dev.kdl and swap the opencode command for an attach wrapper.
            let layout_path = dirs::home_dir()
                .context("No home dir")?
                .join(".config/zellij/layouts/dev.kdl");
            let base_layout = fs::read_to_string(&layout_path)
                .context("Failed to read dev.kdl")?;

            let wrapper_path = std::env::temp_dir()
                .join(format!("meow-attach-{}.sh", &sid[..12.min(sid.len())]));
            fs::write(
                &wrapper_path,
                format!(
                    "#!/usr/bin/env bash\ntrap 'kill {pid} 2>/dev/null' EXIT\nopencode attach {base} --session {sid}\n"
                ),
            )
            .context("Failed to write attach wrapper")?;
            {
                use std::os::unix::fs::PermissionsExt as _;
                let mut perms = fs::metadata(&wrapper_path)?.permissions();
                perms.set_mode(0o755);
                fs::set_permissions(&wrapper_path, perms)?;
            }

            let wrapper_str = wrapper_path.to_string_lossy();
            let kdl = base_layout.replace(
                r#"args "exec" "." "opencode""#,
                &format!(r#"args "exec" "." "{wrapper_str}""#),
            );
            let kdl = inject_tab_name(&kdl, &tab_name, &project_path);

            Ok::<String, anyhow::Error>(kdl)
        })?;

        Some(layout_content)
    } else {
        None
    };

    // Resolve layout: exec-generated > project custom > base layout with tab name injection > named layout
    let layout_content = exec_layout.or_else(|| resolve_layout(&project_path, &tab_name, layout));
    let layout_to_use = match layout_content {
        Some(ref content) => {
            let path = std::env::temp_dir().join(format!("meow-{}.kdl", std::process::id()));
            fs::write(&path, content).context("Failed to write temp layout")?;
            path.to_string_lossy().to_string()
        }
        None => layout.to_string(),
    };

    let inside_zellij = std::env::var("ZELLIJ").is_ok();

    if !inside_zellij {
        // Check for an active zellij session to attach to
        let sessions_output = Command::new("zellij")
            .args(["list-sessions", "-n"]) // -n: no ANSI formatting
            .output()
            .map(|o| String::from_utf8_lossy(&o.stdout).to_string())
            .unwrap_or_default();

        let total_sessions = sessions_output.lines().count();
        let running_sessions: Vec<_> = sessions_output
            .lines()
            .filter(|l| !l.contains("EXITED"))
            .collect();
        debug!(
            total_sessions,
            running = running_sessions.len(),
            "zellij session detection"
        );

        // Find active session (running, not EXITED)
        let active_session = running_sessions
            .iter()
            .find(|l| l.contains("(current)"))
            .or_else(|| running_sessions.first())
            .and_then(|l| l.split_whitespace().next())
            .map(String::from);

        debug!(?active_session, "selected session");

        if let Some(ref session) = active_session {
            // Add tab to existing session (running in another window)
            let _ = Command::new("zellij")
                .args([
                    "--session",
                    session,
                    "action",
                    "new-tab",
                    "--layout",
                    &layout_to_use,
                    "--name",
                    &tab_name,
                    "-c",
                    &project_path.to_string_lossy(),
                ])
                .status()
                .context("Failed to create tab in existing session")?;

            #[allow(clippy::print_stdout)]
            {
                println!("Added {} to session '{}'", tab_name, session);
            }
            return Ok(());
        }

        // No running session: spawn new one with layout
        use std::os::unix::process::CommandExt;
        let err = Command::new("zellij")
            .args(["-l", &layout_to_use])
            .current_dir(&project_path)
            .exec();
        bail!("Failed to exec zellij: {}", err);
    }

    // Inside zellij: use action commands
    let output = Command::new("zellij")
        .args(["action", "query-tab-names"])
        .output()
        .context("Failed to query zellij tabs")?;

    let tab_names: Vec<&str> = std::str::from_utf8(&output.stdout)
        .unwrap_or("")
        .lines()
        .collect();

    if !use_worktree && tab_names.contains(&project.as_str()) {
        // Only switch to existing tab if not using worktree (worktree always creates new)
        let _ = Command::new("zellij")
            .args(["action", "go-to-tab-name", &project])
            .status()
            .context("Failed to switch zellij tab")?;
    } else {
        let _ = Command::new("zellij")
            .args([
                "action",
                "new-tab",
                "--layout",
                &layout_to_use,
                "--name",
                &tab_name,
                "-c",
                &project_path.to_string_lossy(),
            ])
            .status()
            .context("Failed to create zellij tab")?;
    }

    Ok(())
}

/// Exit code indicating mirror is already synced (no work needed).
pub const EXIT_ALREADY_SYNCED: u8 = 2;

/// Exit code indicating secrets were found in the mirror.
pub const EXIT_SECRETS_FOUND: u8 = 3;

/// Result of mirror command - either success, already synced, or error.
pub fn cmd_mirror(root: &Path, command: MirrorCommand, use_color: bool) -> Result<Option<u8>> {
    match command {
        MirrorCommand::Status => cmd_mirror_status(root, use_color).map(|()| None),
        MirrorCommand::Diff { project, no_scan } => cmd_mirror_diff(root, project, no_scan),
        MirrorCommand::Push { project, message } => {
            cmd_mirror_push(root, project, &message).map(|()| None)
        }
    }
}

fn cmd_mirror_status(root: &Path, use_color: bool) -> Result<()> {
    let statuses = mirror::get_all_status(root)?;

    if statuses.is_empty() {
        info!(
            "No projects configured for mirroring (add 'github: org/repo' to README frontmatter)"
        );
        return Ok(());
    }

    let table = render_mirror_table(&statuses);
    markdown::skin(use_color).print_text(&table);
    Ok(())
}

fn humanize_age(unsynced_since: i64) -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    let secs = (now - unsynced_since).max(0) as u64;
    if secs < 3600 {
        let mins = secs / 60;
        format!("{} min ago", mins.max(1))
    } else if secs < 86400 {
        let hours = secs / 3600;
        format!("{} hour{} ago", hours, if hours == 1 { "" } else { "s" })
    } else {
        let days = secs / 86400;
        format!("{} day{} ago", days, if days == 1 { "" } else { "s" })
    }
}

fn render_mirror_table(statuses: &[mirror::MirrorStatus]) -> String {
    let mut md = String::new();
    md.push_str("| Project | Code | Templates | Alerts |\n");
    md.push_str("|:--------|:-----|:----------|:-------|\n");

    for s in statuses {
        let code = match s.commits_ahead {
            None => "?".to_string(),
            Some(0) => "synced".to_string(),
            Some(n) => {
                let age = s
                    .unsynced_since
                    .map(humanize_age)
                    .unwrap_or_else(|| "?".to_string());
                format!("{} commit{} ({})", n, if n == 1 { "" } else { "s" }, age)
            }
        };

        let templates = if s.templates_stale { "stale" } else { "ok" };

        let alerts = github::format_alerts(&s.dependabot_alerts);

        md.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            s.project, code, templates, alerts
        ));
    }

    md
}

fn cmd_mirror_diff(root: &Path, project: Option<String>, no_scan: bool) -> Result<Option<u8>> {
    // Resolve project name
    let project = match project {
        Some(p) => p,
        None => git::detect_project_from_cwd(root)?
            .context("Not in a project directory. Specify project name.")?,
    };

    // HARD GATE: Check for uncommitted changes in project directory
    if mirror::is_project_dirty(root, &project) {
        bail!(
            "Project '{}' has uncommitted changes. Commit or stash before mirroring.",
            project
        );
    }

    // Load project README and extract mirror config
    let readme_path = root.join(PROJECTS_DIR).join(&project).join("README.md");
    if !readme_path.exists() {
        bail!("Project '{}' has no README.md", project);
    }

    let content = fs::read_to_string(&readme_path)?;
    let fm = parse_frontmatter(&content);
    let config = fm
        .as_ref()
        .and_then(mirror::MirrorConfig::from_frontmatter)
        .with_context(|| {
            format!(
                "Project '{}' has no github field in README frontmatter",
                project
            )
        })?;

    // Validate workflow names before doing any work
    config.validate_workflows()?;

    // Ensure clone exists
    let mirror_path = mirror::ensure_clone(&config.org, &config.repo)?;

    // Check if sync is needed (both commit and template hash must match)
    let source_path = root.join(PROJECTS_DIR).join(&project);
    let latest_commit = mirror::get_latest_project_commit(root, &project)?;
    let setup_steps = mirror::read_setup_steps(&source_path)?;
    let expected_template_hash =
        mirror::compute_template_hash(&config.workflows, setup_steps.as_deref());
    if let Some(sync_info) = mirror::get_sync_info(&mirror_path) {
        if sync_info.commit == latest_commit && sync_info.templates_hash == expected_template_hash {
            #[allow(clippy::print_stdout)]
            {
                println!("{}", mirror_path.display());
            }
            info!(
                "Already synced at {}",
                &latest_commit[..8.min(latest_commit.len())]
            );
            return Ok(Some(EXIT_ALREADY_SYNCED));
        }
    }

    // Sync filtered content to mirror
    let file_count = mirror::sync_to_mirror(&source_path, &mirror_path, &config)?;

    // Write sync info for shipit-mirror to use when creating the tag
    let sync_info = mirror::SyncInfo {
        commit: latest_commit.clone(),
        templates_hash: expected_template_hash,
    };
    mirror::write_sync_info(&mirror_path, &sync_info)?;

    #[allow(clippy::print_stdout)]
    {
        println!("{}", mirror_path.display());
    }

    info!(
        "Synced {} files to mirror (excludes: {:?})",
        file_count,
        mirror::HARDCODED_EXCLUDES
    );

    // Run secret scan unless --no-scan was passed
    if !no_scan {
        if let Some(exit_code) = run_trufflehog_scan(&mirror_path)? {
            return Ok(Some(exit_code));
        }
    }

    Ok(None)
}

fn cmd_mirror_push(root: &Path, project: Option<String>, message: &str) -> Result<()> {
    let project = match project {
        Some(p) => p,
        None => git::detect_project_from_cwd(root)?
            .context("Not in a project directory. Specify project name.")?,
    };

    // Load mirror config from README frontmatter
    let readme_path = root.join(PROJECTS_DIR).join(&project).join("README.md");
    if !readme_path.exists() {
        bail!("Project '{}' has no README.md", project);
    }

    let content = fs::read_to_string(&readme_path)?;
    let fm = parse_frontmatter(&content);
    let config = fm
        .as_ref()
        .and_then(mirror::MirrorConfig::from_frontmatter)
        .with_context(|| {
            format!(
                "Project '{}' has no github field in README frontmatter",
                project
            )
        })?;

    let mirror_path = mirror::mirror_path(&config.org, &config.repo)?;
    if !mirror_path.exists() {
        bail!(
            "Mirror clone not found at {}. Run 'meow mirror diff' first.",
            mirror_path.display()
        );
    }

    // Guard: ensure mirror diff was run (sync info file must exist)
    let sync_info_path = mirror_path.join(".git").join("meow-sync-info");
    if !sync_info_path.exists() {
        bail!("No sync info found. Run 'meow mirror diff' before pushing.");
    }

    // Commit, push, and update tracking tag
    let result = mirror::push_mirror(&mirror_path, message)?;

    info!(
        "Committed {} \"{}\"",
        result.commit_hash.as_deref().unwrap_or("?"),
        message
    );
    info!("Pushed to {}/{} main", config.org, config.repo);

    if result.tracking_tag_updated {
        info!("Updated tracking tag (monorepo-synced)");
    } else if let Some(ref err) = result.tracking_tag_error {
        warn!("Tracking tag failed: {}", err);
    }

    // Sync repo description from frontmatter
    if let Some(desc) = fm.as_ref().and_then(|f| f.description.as_ref()) {
        match mirror::sync_repo_description(&config.org, &config.repo, desc) {
            Ok(()) => info!("Synced repo description"),
            Err(e) => warn!("Description sync failed: {}", e),
        }
    }

    // Version tagging
    if let Some(version) = mirror::read_project_version(root, &project) {
        match mirror::tag_version_if_needed(&mirror_path, &version) {
            Ok(mirror::VersionTagResult::Created(tag)) => info!("Tagged {}", tag),
            Ok(mirror::VersionTagResult::AlreadyExists(tag)) => {
                info!("{} already tagged — skipped", tag)
            }
            Err(e) => warn!("Version tagging failed: {}", e),
        }
    }

    Ok(())
}

pub fn cmd_web(
    _repo: &git2::Repository,
    root: &Path,
    query: Option<String>,
    command: Option<WebCommand>,
    sandbox: Option<bool>,
    use_color: bool,
) -> Result<Option<u8>> {
    let runtime = tokio::runtime::Runtime::new().context("Failed to create async runtime")?;

    match command {
        Some(WebCommand::List) => {
            runtime.block_on(async {
                match web::running_proxy_port().await {
                    None => info!("No meow web proxy running"),
                    Some(port) => {
                        let sessions: Vec<web::SessionInfo> = proxy_api_get(port, "sessions")
                            .await
                            .context("Failed to reach proxy")?
                            .json()
                            .await
                            .context("Failed to parse sessions")?;
                        if sessions.is_empty() {
                            info!("No active sessions");
                        } else {
                            #[allow(clippy::print_stdout)]
                            for s in &sessions {
                                println!("{} (port {})", s.project, s.port);
                            }
                        }
                    }
                }
                Ok::<(), anyhow::Error>(())
            })?;
            Ok(None)
        }

        Some(WebCommand::Stop { query: stop_query }) => {
            let focused = sparse::get_focused_projects(root)?;
            if focused.is_empty() {
                bail!("No projects currently focused");
            }
            let project = resolve_project(stop_query, focused, use_color)?;
            runtime.block_on(async {
                let port = web::running_proxy_port()
                    .await
                    .context("No meow web proxy running")?;
                let resp = proxy_api_post(port, "stop", serde_json::json!({"project": project}))
                    .await
                    .context("Failed to reach proxy")?;
                if resp.status().is_success() {
                    info!("Stopped '{}'", project);
                } else {
                    let body: serde_json::Value = resp.json().await.unwrap_or_default();
                    bail!("{}", body["error"].as_str().unwrap_or("unknown error"));
                }
                Ok(())
            })?;
            Ok(None)
        }

        None => {
            let mut web_config = crate::config::Config::load()?.web;

            // Apply --sandbox CLI override.
            if let Some(want_sandbox) = sandbox {
                if want_sandbox {
                    match web_config.sandbox {
                        Some(ref mut s) => s.enabled = true,
                        None => bail!(
                            "--sandbox requires sandbox config in config.yaml \
                             (web.sandbox.nj_bin, alice_bin, sandbox_package)"
                        ),
                    }
                } else {
                    web_config.sandbox = None;
                }
            }

            // No query and no subcommand → open the home landing page
            if query.is_none() {
                let url = home_url(&web_config);
                let existing_proxy = runtime.block_on(web::running_proxy_port());
                if let Some(_port) = existing_proxy {
                    open_chrome(&url);
                } else {
                    // No proxy running - we are the server.
                    // Start with a dummy project that will never be used before the
                    // browser opens; the landing page handles everything from there.
                    info!("Starting meow web proxy");
                    let dummy_project = String::new();
                    let dummy_path = root.to_path_buf();
                    runtime.block_on(async {
                        let (ready_tx, ready_rx) = tokio::sync::oneshot::channel::<()>();
                        let server_task = tokio::spawn(web::run_server(
                            web_config,
                            dummy_project,
                            dummy_path,
                            root.to_path_buf(),
                            Some(ready_tx),
                        ));
                        let _ = ready_rx.await;
                        open_chrome(&url);
                        server_task.await.context("server task panicked")??;
                        Ok::<(), anyhow::Error>(())
                    })?;
                }
                return Ok(None);
            }

            let focused = sparse::get_focused_projects(root)?;
            if focused.is_empty() {
                bail!("No projects currently focused");
            }
            let project = resolve_project(query, focused, use_color)?;
            let project_path = root.join(crate::PROJECTS_DIR).join(&project);
            let project_url = project_url(&web_config, &project);

            let existing_proxy = runtime.block_on(web::running_proxy_port());

            if let Some(port) = existing_proxy {
                // Proxy already running - add project and open browser, then exit
                runtime.block_on(async {
                    let resp = proxy_api_post(
                        port,
                        "add",
                        serde_json::json!({
                            "project": project,
                            "path": project_path.display().to_string(),
                        }),
                    )
                    .await
                    .context("Failed to reach proxy")?;
                    if resp.status().is_success() {
                        info!("Added '{}' to running proxy", project);
                    } else {
                        let body: serde_json::Value = resp.json().await.unwrap_or_default();
                        bail!("{}", body["error"].as_str().unwrap_or("unknown error"));
                    }
                    Ok::<(), anyhow::Error>(())
                })?;
                open_chrome(&project_url);
            } else {
                // No proxy running - we are the server.
                // Start the server, wait until it's ready, then open the browser.
                info!("Starting meow web proxy for '{}'", project);
                runtime.block_on(async {
                    let (ready_tx, ready_rx) = tokio::sync::oneshot::channel::<()>();
                    let server_task = tokio::spawn(web::run_server(
                        web_config,
                        project.clone(),
                        project_path.clone(),
                        root.to_path_buf(),
                        Some(ready_tx),
                    ));
                    // Wait for the server to signal it's listening before opening browser
                    let _ = ready_rx.await;
                    open_chrome(&project_url);
                    // Now block until the server exits
                    server_task.await.context("server task panicked")??;
                    Ok::<(), anyhow::Error>(())
                })?;
            }
            Ok(None)
        }
    }
}

/// Pick the "primary" host for URL construction: prefer TLS hosts, otherwise
/// use the first configured host.
fn primary_host(web_config: &crate::config::WebConfig) -> &crate::config::HostConfig {
    web_config
        .tls_hosts()
        .into_iter()
        .next()
        .unwrap_or_else(|| &web_config.hosts[0])
}

/// Build the URL for the landing page on the meow web proxy.
fn home_url(web_config: &crate::config::WebConfig) -> String {
    let hc = primary_host(web_config);
    let tls = hc.tls_enabled();
    let scheme = if tls { "https" } else { "http" };
    let port = if tls {
        web_config.port
    } else {
        web_config.http_port
    };
    let port_suffix = match (tls, port) {
        (true, 443) | (false, 80) => String::new(),
        (_, p) => format!(":{}", p),
    };
    format!("{}://home.{}{}/", scheme, hc.hostname, port_suffix)
}

fn project_url(web_config: &crate::config::WebConfig, project: &str) -> String {
    let hc = primary_host(web_config);
    let tls = hc.tls_enabled();
    let scheme = if tls { "https" } else { "http" };
    let port = if tls {
        web_config.port
    } else {
        web_config.http_port
    };
    let port_suffix = match (tls, port) {
        (true, 443) | (false, 80) => String::new(),
        (_, p) => format!(":{}", p),
    };
    format!("{}://{}.{}{}/", scheme, project, hc.hostname, port_suffix)
}

/// Open a URL in Chrome Beta if available, falling back to xdg-open.
/// When running inside opencode (OPENCODE=1), prints a message for the
/// model instead of attempting to launch a browser.
fn open_chrome(url: &str) {
    if std::env::var("OPENCODE").is_ok() {
        #[allow(clippy::print_stdout)]
        {
            println!("Session open at {}", url);
        }
        return;
    }

    // Try macOS `open -a "Google Chrome Beta"` first
    if cfg!(target_os = "macos") {
        let status = Command::new("open")
            .args(["-a", "Google Chrome Beta", url])
            .status();
        match status {
            Ok(s) if s.success() => return,
            _ => {} // fall through to xdg-open
        }
    }

    // Try xdg-open (Linux / fallback)
    if let Err(e) = Command::new("xdg-open").arg(url).status() {
        warn!("Failed to open browser: {}", e);
    }
}

/// Resolve a project from an optional fuzzy query + list of candidates.
/// Exact matches are returned immediately without showing the picker.
fn resolve_project(
    query: Option<String>,
    candidates: Vec<String>,
    use_color: bool,
) -> Result<String> {
    picker::fuzzy_pick(candidates, query.as_deref(), use_color)?.context("No project selected")
}

/// Run trufflehog secret scan on the mirror directory.
/// Returns Some(EXIT_SECRETS_FOUND) if secrets were detected, None if clean.
fn run_trufflehog_scan(mirror_path: &Path) -> Result<Option<u8>> {
    use std::process::Command;

    let output = Command::new("trufflehog")
        .args(["filesystem", "--fail"])
        .arg(mirror_path)
        .output();

    let output = match output {
        Ok(o) => o,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            bail!(
                "trufflehog not found in PATH. Install trufflehog or use --no-scan to skip secret scanning."
            );
        }
        Err(e) => {
            return Err(e).context("Failed to run trufflehog");
        }
    };

    match output.status.code() {
        Some(0) => {
            info!("Secret scan passed");
            Ok(None)
        }
        Some(183) => {
            // Secrets found - trufflehog already printed findings to stdout/stderr
            #[allow(clippy::print_stderr)]
            {
                // Ensure trufflehog output is visible
                let stdout = String::from_utf8_lossy(&output.stdout);
                let stderr = String::from_utf8_lossy(&output.stderr);
                if !stdout.is_empty() {
                    eprintln!("{}", stdout);
                }
                if !stderr.is_empty() {
                    eprintln!("{}", stderr);
                }
                eprintln!("Secret scan failed: potential secrets detected in mirror");
            }
            Ok(Some(EXIT_SECRETS_FOUND))
        }
        Some(code) => {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("trufflehog exited with code {}: {}", code, stderr.trim());
        }
        None => {
            bail!("trufflehog was terminated by signal");
        }
    }
}

use anyhow::{bail, Context, Result};
use chrono::NaiveDate;
use git2::Repository;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::{debug, info, warn};

use crate::cli::{JournalArgs, MirrorCommand, PruneArgs};
use crate::markdown::{self, parse_frontmatter};
use crate::{git, github, mirror, picker, sparse, PROJECTS_DIR};

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
    case list ls rm drop init journal zellij z prune pull mirror please plz qmd --help -h --version -V
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

function mp
  meow please $argv
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
                    if let Some(port) = content.lines().find_map(extract_port) {
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

// ---------------------------------------------------------------------------
// QMD integration
// ---------------------------------------------------------------------------

/// A project that should be indexed by QMD.
struct QmdProject {
    name: String,
    path: PathBuf,
    description: String,
    mode: crate::markdown::QmdMode,
}

/// Run a qmd subcommand, returning its stdout as a String.
fn qmd_run(args: &[&str]) -> Result<String> {
    let out = Command::new("qmd")
        .args(args)
        .output()
        .context("Failed to run qmd (is it installed?)")?;
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        bail!("qmd {} failed: {}", args.join(" "), stderr.trim());
    }
    Ok(String::from_utf8_lossy(&out.stdout).into_owned())
}

/// List collection names currently registered in QMD.
fn qmd_collection_names() -> Result<Vec<String>> {
    let out = Command::new("qmd")
        .args(["collection", "list"])
        .output()
        .context("Failed to run qmd (is it installed?)")?;

    // qmd exits 0 with "No collections" message when empty — treat non-zero as error
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr);
        // Non-fatal: if qmd isn't set up yet, treat as empty
        debug!("qmd collection list failed: {}", stderr.trim());
        return Ok(vec![]);
    }

    let stdout = String::from_utf8_lossy(&out.stdout);
    // Output format: each collection starts a block with "name (qmd://name/)"
    // Parse lines that match: word followed by whitespace and "(qmd://...)"
    let names = stdout
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            // Lines like: "knowledge (qmd://knowledge/)" — take first word
            let name = trimmed.split_whitespace().next()?;
            // Only keep if followed by "(qmd://"
            if trimmed.contains("(qmd://") && !name.starts_with('(') {
                Some(name.to_string())
            } else {
                None
            }
        })
        .collect();
    Ok(names)
}

/// Collect focused projects that have qmd: configured.
fn qmd_desired_projects(repo: &git2::Repository, root: &Path) -> Result<Vec<QmdProject>> {
    let focused = sparse::get_focused_projects(root)?;
    let mut result = Vec::new();
    for name in focused {
        let fm = markdown::load_readme_frontmatter(repo, root, &name);
        let mode = fm.as_ref().and_then(|f| f.qmd_mode());
        if let Some(mode) = mode {
            result.push(QmdProject {
                path: root.join(crate::PROJECTS_DIR).join(&name),
                description: fm
                    .as_ref()
                    .and_then(|f| f.description.clone())
                    .unwrap_or_default(),
                name,
                mode,
            });
        }
    }
    Ok(result)
}

pub fn cmd_qmd_sync(repo: &git2::Repository, root: &Path) -> Result<()> {
    let desired = qmd_desired_projects(repo, root)?;
    let existing = qmd_collection_names()?;

    let desired_names: Vec<&str> = desired.iter().map(|p| p.name.as_str()).collect();

    // Add missing collections
    let mut needs_embed = false;
    for project in &desired {
        if existing.contains(&project.name) {
            debug!("qmd: {} already indexed", project.name);
        } else {
            info!("qmd: adding collection '{}'", project.name);
            let _ = qmd_run(&[
                "collection",
                "add",
                &project.path.to_string_lossy(),
                "--name",
                &project.name,
            ])?;

            if !project.description.is_empty() {
                let vpath = format!("qmd://{}/", project.name);
                // Non-fatal: context add may fail if qmd isn't fully set up
                if let Err(e) = qmd_run(&["context", "add", &vpath, &project.description]) {
                    warn!("qmd: context add failed for {}: {}", project.name, e);
                }
            }

            if project.mode == crate::markdown::QmdMode::Embed {
                needs_embed = true;
            }
        }
    }

    // Remove collections for projects no longer focused or no longer qmd-enabled,
    // but only touch collections whose name matches a known meow project
    // (leave manually-added collections like 'knowledge' alone).
    let all_projects = git::list_all_projects(repo)?;
    for name in &existing {
        if all_projects.contains(name) && !desired_names.contains(&name.as_str()) {
            info!("qmd: removing collection '{}'", name);
            let _ = qmd_run(&["collection", "remove", name])?;
        }
    }

    // Run embed for projects that requested it
    if needs_embed {
        info!("qmd: running embed");
        let status = Command::new("qmd")
            .arg("embed")
            .status()
            .context("Failed to run qmd embed")?;
        if !status.success() {
            bail!("qmd embed failed");
        }
    }

    Ok(())
}

pub fn cmd_qmd_status(repo: &git2::Repository, root: &Path, use_color: bool) -> Result<()> {
    let desired = qmd_desired_projects(repo, root)?;
    let existing = qmd_collection_names()?;

    let mut md = String::new();
    md.push_str("| Project | qmd | Indexed |\n");
    md.push_str("|:--------|:----|:--------|\n");

    for project in &desired {
        let indexed = if existing.contains(&project.name) {
            "yes"
        } else {
            "no"
        };
        let mode = match project.mode {
            crate::markdown::QmdMode::Index => "true",
            crate::markdown::QmdMode::Embed => "embed",
        };
        md.push_str(&format!("| {} | {} | {} |\n", project.name, mode, indexed));
    }

    if desired.is_empty() {
        info!("No focused projects have qmd: configured");
    } else {
        markdown::skin(use_color).print_text(&md);
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
    let template_vars = mirror::resolve_template_vars(root, &config.workflows)?;
    let expected_template_hash =
        mirror::compute_template_hash(&config.workflows, setup_steps.as_deref(), &template_vars);
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
    let file_count = mirror::sync_to_mirror(&source_path, &mirror_path, &config, &template_vars)?;

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

/// Submit projects to nix-jail via nj-web and open the browser.
pub fn cmd_agent(
    repo: &git2::Repository,
    root: &Path,
    projects: Vec<String>,
    profiles: Vec<String>,
    branch: Option<String>,
    no_cleanup: bool,
    use_color: bool,
) -> Result<()> {
    let config = crate::config::Config::load()?;

    // Resolve project names. With no args, use interactive picker over all projects.
    let all_projects = git::list_all_projects(repo)?;

    let resolved: Vec<String> = if projects.is_empty() {
        vec![resolve_project(None, all_projects, use_color)?]
    } else {
        projects
            .into_iter()
            .map(|q| resolve_project(Some(q), all_projects.clone(), use_color))
            .collect::<Result<Vec<_>>>()?
    };

    if resolved.is_empty() {
        bail!("No project selected");
    }

    let primary = &resolved[0];
    let primary_path = format!("{PROJECTS_DIR}/{primary}");
    let extra_paths: Vec<String> = resolved[1..]
        .iter()
        .map(|p| format!("{PROJECTS_DIR}/{p}"))
        .collect();

    // Auto-detect profiles when not overridden.
    let profiles = if profiles.is_empty() {
        let project_dir = root.join(PROJECTS_DIR).join(primary);
        let mut detected = vec!["opencode".to_string()];
        if project_dir.join("Cargo.toml").exists() {
            detected.push("cargo".to_string());
        }
        detected
    } else {
        profiles
    };

    // Get git remote origin URL for the repo, normalised to HTTPS (nj-web
    // requires an HTTPS URL for cloning; SSH remotes are converted).
    let remote = repo
        .find_remote("origin")
        .context("No remote 'origin' found")?;
    let repo_url = ssh_to_https(remote.url().context("Remote 'origin' has no URL")?);

    // Use the primary project name as the base subdomain (DNS-safe label).
    let subdomain = primary.replace('_', "-").to_ascii_lowercase();

    let body = serde_json::json!({
        "profiles": profiles,
        "repo": repo_url,
        "path": primary_path,
        "extra_paths": extra_paths,
        "cwd": primary_path,
        "push": true,
        "git_ref": branch,
        "subdomain": subdomain,
        "no_cleanup": no_cleanup,
    });

    info!(
        primary = %primary,
        profiles = ?body["profiles"],
        "submitting agent job to nj-web"
    );

    let runtime = tokio::runtime::Runtime::new().context("Failed to create async runtime")?;
    let response: serde_json::Value = runtime.block_on(async {
        let client = reqwest::Client::new();
        let url = format!("{}/api/jobs", config.nj_web_url);
        let resp = client
            .post(&url)
            .json(&body)
            .send()
            .await
            .context("Failed to POST to nj-web")?;
        let status = resp.status();
        info!(status = status.as_u16(), "nj-web response");
        resp.error_for_status()
            .context("nj-web returned error status")?
            .json()
            .await
            .context("Failed to parse nj-web response")
    })?;

    if let Some(err) = response["error"].as_str() {
        bail!("nj-web rejected job: {err}");
    }

    let job_id = response["job_id"]
        .as_str()
        .context("No job_id in nj-web response")?;

    info!(job_id, "job submitted");

    Ok(())
}

/// Convert an SSH remote URL to HTTPS so nj-web can clone it.
///
/// Handles:
/// - `ssh://user@host:port/path` → `https://host/path`
/// - `git@host:path`             → `https://host/path`
/// - `https://…`                 → unchanged
fn ssh_to_https(url: &str) -> String {
    // ssh://[user@]host[:port]/path
    if let Some(rest) = url.strip_prefix("ssh://") {
        let after_user = rest.find('@').map(|i| &rest[i + 1..]).unwrap_or(rest);
        // strip optional :port
        let path_start = after_user.find('/').unwrap_or(after_user.len());
        let host = &after_user[..path_start];
        let host = host.find(':').map(|i| &host[..i]).unwrap_or(host);
        let path = &after_user[path_start..];
        return format!("https://{host}{path}");
    }
    // git@host:path
    if let Some(rest) = url.strip_prefix("git@") {
        if let Some(colon) = rest.find(':') {
            let host = &rest[..colon];
            let path = &rest[colon + 1..];
            return format!("https://{host}/{path}");
        }
    }
    url.to_owned()
}

#[cfg(test)]
mod ssh_to_https_tests {
    use super::ssh_to_https;

    #[test]
    fn test_ssh_with_user_and_port() {
        assert_eq!(
            ssh_to_https("ssh://git@git.example.com:2222/org/repo.git"),
            "https://git.example.com/org/repo.git"
        );
    }

    #[test]
    fn test_ssh_plain() {
        assert_eq!(
            ssh_to_https("ssh://git.example.com/org/repo.git"),
            "https://git.example.com/org/repo.git"
        );
    }

    #[test]
    fn test_git_at_scp() {
        assert_eq!(
            ssh_to_https("git@github.com:org/repo.git"),
            "https://github.com/org/repo.git"
        );
    }

    #[test]
    fn test_https_passthrough() {
        assert_eq!(
            ssh_to_https("https://git.example.com/org/repo.git"),
            "https://git.example.com/org/repo.git"
        );
    }
}

/// Extract a port number from a log line (e.g. "listening on :3000").
/// Matches a 4-5 digit port preceded by `:` and followed by `/`, whitespace, or end.
fn extract_port(line: &str) -> Option<u16> {
    let re = regex::Regex::new(r":(\d{4,5})(?:[/\s]|$)").ok()?;
    let caps = re.captures(line)?;
    caps.get(1)?.as_str().parse().ok()
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

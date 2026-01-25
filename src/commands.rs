use anyhow::{bail, Context, Result};
use chrono::NaiveDate;
use git2::Repository;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tracing::{debug, info, warn};

use crate::cli::{FmtArgs, JournalArgs, MirrorCommand, PruneArgs};
use crate::markdown::{
    self, claude, is_encrypted, parse, parse_encrypted, readme, serialize, FormatContext,
};
use crate::{git, mirror, picker, sparse, PROJECTS_DIR};

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

struct ProjectInfo {
    name: String,
    description: String,
    created: Option<NaiveDate>,
    updated: Option<NaiveDate>,
    focused: bool,
}

fn gather_project_info(
    repo: &Repository,
    root: &Path,
    projects: &[String],
    focused: &[String],
) -> Result<Vec<ProjectInfo>> {
    projects
        .iter()
        .map(|name| {
            let readme_doc = readme::load_readme(repo, root, name).ok();
            let fm = readme_doc.as_ref().and_then(|d| d.frontmatter.as_ref());

            Ok(ProjectInfo {
                name: name.clone(),
                description: fm.and_then(|f| f.description.clone()).unwrap_or_default(),
                created: fm.and_then(|f| f.created),
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

    let selected = match query {
        Some(q) => {
            let matches = picker::fuzzy_match(&projects, &q);
            if matches.len() == 1 {
                Some(matches[0].clone())
            } else if matches.is_empty() {
                picker::pick(projects, use_color)?
            } else {
                picker::pick(matches, use_color)?
            }
        }
        None => picker::pick(projects, use_color)?,
    };

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
    case list ls rm drop init fmt journal zellij z prune decrypt pull mirror --help -h --version -V
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

pub fn cmd_fmt(repo: &Repository, root: &Path, args: FmtArgs) -> Result<()> {
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

    // Load git tree for link validation (works on files not in sparse checkout)
    let git_tree = git::list_all_paths(repo)?;
    let mut has_errors = false;
    let mut needs_formatting = false;

    let opts = markdown::FormatOptions {
        skip_encrypted: args.skip_encrypted,
        check: args.check,
    };

    for project in &projects {
        let result = markdown::format_project(root, project, Some(&git_tree), opts)?;
        if print_format_result(project, &result, args.check).is_err() {
            has_errors = true;
        }
        if result.files_formatted > 0 {
            needs_formatting = true;
        }
    }

    if has_errors {
        anyhow::bail!("formatting failed")
    }
    if args.check && needs_formatting {
        anyhow::bail!("files need formatting")
    }
    Ok(())
}

fn print_format_result(project: &str, result: &markdown::FormatResult, check: bool) -> Result<()> {
    #[allow(clippy::print_stdout)]
    {
        if result.files_checked == 0 {
            println!("No markdown files found for {project}");
            return Ok(());
        }

        if check {
            println!("Checking {project}...");
        } else {
            println!("Formatting {project}...");
        }

        for error in &result.errors {
            for e in &error.errors {
                if e.line > 0 {
                    println!("  {}:{}: {}", error.path, e.line, e.message);
                } else {
                    println!("  {}: {}", error.path, e.message);
                }
            }
        }

        if result.errors.is_empty() {
            if result.files_formatted > 0 {
                let verb = if check {
                    "need formatting"
                } else {
                    "formatted"
                };
                println!(
                    "{} files checked, {} {}",
                    result.files_checked, result.files_formatted, verb
                );
            } else {
                println!("{} files checked, all ok", result.files_checked);
            }
        } else {
            println!("{} files with errors", result.errors.len());
        }
    }

    if result.errors.is_empty() {
        Ok(())
    } else {
        anyhow::bail!("formatting failed")
    }
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
            found_any = true;
            let md = markdown::render_timeline(project, &timeline);
            markdown::skin(use_color).print_text(&md);
        } else {
            // Original behavior: just journal entries
            if entries.is_empty() {
                continue;
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

    let claude_path = project_dir.join("CLAUDE.md");
    let mut claude_doc = parse("");
    let claude_ctx = FormatContext {
        project,
        path: &claude_path,
        year_month: None,
        git_tree: None,
        repo_root: None,
    };
    claude::normalize(&mut claude_doc, &claude_ctx);
    fs::write(&claude_path, serialize(&claude_doc)).context("Failed to create CLAUDE.md")?;

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

pub fn cmd_zellij(
    root: &Path,
    query: Option<String>,
    branch_parts: Vec<String>,
    layout: &str,
    worktree: bool,
    use_color: bool,
) -> Result<()> {
    let focused = sparse::get_focused_projects(root)?;
    if focused.is_empty() {
        bail!("No projects currently focused");
    }

    let selected = match query {
        Some(q) => {
            let matches = picker::fuzzy_match(&focused, &q);
            match matches.len() {
                1 => Some(matches[0].clone()),
                0 => picker::pick(focused, use_color)?,
                _ => picker::pick(matches, use_color)?,
            }
        }
        None => picker::pick(focused, use_color)?,
    };

    let project = selected.context("No project selected")?;

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

    // Resolve layout: project custom > base layout with tab name injection > named layout
    let layout_to_use = match resolve_layout(&project_path, &tab_name, layout) {
        Some(content) => {
            let path = std::env::temp_dir().join(format!("meow-{}.kdl", std::process::id()));
            fs::write(&path, &content).context("Failed to write temp layout")?;
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

/// Check if stdout is safe for decrypted content.
/// Allows: TTY (terminal), or non-file output when running under Claude Code.
/// Blocks: regular files (redirects like `> file.txt`).
fn is_stdout_allowed() -> bool {
    use std::io::IsTerminal;

    // TTY is always allowed
    if std::io::stdout().is_terminal() {
        return true;
    }

    // Check stdout type via /dev/fd/1
    let is_regular_file = std::fs::metadata("/dev/fd/1")
        .map(|m| m.file_type().is_file())
        .unwrap_or(false);

    // Block regular files (redirects)
    // Allow pipes/sockets/etc only under Claude Code
    if is_regular_file {
        false
    } else {
        std::env::var("CLAUDECODE").is_ok()
    }
}

pub fn cmd_decrypt(file: &Path, in_place: bool) -> Result<()> {
    let content =
        fs::read_to_string(file).with_context(|| format!("failed to read {}", file.display()))?;

    if !is_encrypted(&content) {
        bail!("file is not encrypted: {}", file.display());
    }

    let (doc, _original_body) = parse_encrypted(&content)?;
    let decrypted = serialize(&doc);

    if in_place {
        fs::write(file, &decrypted)
            .with_context(|| format!("failed to write {}", file.display()))?;
        info!("Decrypted {}", file.display());
    } else {
        if !is_stdout_allowed() {
            bail!("refusing to write decrypted content to file (use -i to decrypt in-place)");
        }
        #[allow(clippy::print_stdout)]
        {
            print!("{}", decrypted);
        }
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

fn render_mirror_table(statuses: &[mirror::MirrorStatus]) -> String {
    let mut md = String::new();
    md.push_str("| Project | Remote | Public | Private |\n");
    md.push_str("|:--------|:-------|:-------|:--------|\n");

    for s in statuses {
        let public = s.public_commit.as_ref().map(|c| &c[..7]).unwrap_or("?");

        let private = match s.commits_ahead {
            Some(0) => s.private_commit[..7].to_string(),
            Some(n) => format!("{} (+{})", &s.private_commit[..7], n),
            None => format!("{} (+?)", &s.private_commit[..7]),
        };

        md.push_str(&format!(
            "| {} | {}/{} | {} | {} |\n",
            s.project, s.config.org, s.config.repo, public, private
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
    let doc = parse(&content);
    let config = doc
        .frontmatter
        .as_ref()
        .and_then(mirror::MirrorConfig::from_frontmatter)
        .with_context(|| {
            format!(
                "Project '{}' has no github field in README frontmatter",
                project
            )
        })?;

    // Ensure clone exists
    let mirror_path = mirror::ensure_clone(&config.org, &config.repo)?;

    // Check if sync is needed
    let latest_commit = mirror::get_latest_project_commit(root, &project)?;
    if let Some(synced_commit) = mirror::get_synced_commit(&mirror_path) {
        if synced_commit == latest_commit {
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
    let source_path = root.join(PROJECTS_DIR).join(&project);
    let file_count = mirror::sync_to_mirror(&source_path, &mirror_path, &config)?;

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

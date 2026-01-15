use anyhow::{Context, Result};
use chrono::{Local, NaiveDate};
use git2::Repository;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::config::{self, Config};
use crate::markdown::journal::CommitEntry;
use crate::PROJECTS_DIR;

/// Returns the directory for git worktrees.
pub fn worktree_dir() -> Result<PathBuf> {
    Ok(config::data_dir()?.join("trees"))
}

pub fn find_repo() -> Result<Repository> {
    let config = Config::load()?;
    Repository::open(&config.repo_path).context("Failed to open repository")
}

pub fn repo_root(repo: &Repository) -> Result<PathBuf> {
    repo.workdir()
        .map(|p| p.to_path_buf())
        .context("Repository has no working directory")
}

pub fn list_all_projects(repo: &Repository) -> Result<Vec<String>> {
    let head = repo.head().context("Failed to get HEAD")?;
    let tree = head
        .peel_to_tree()
        .context("Failed to get tree from HEAD")?;

    let projects_entry = match tree.get_name(PROJECTS_DIR) {
        Some(entry) => entry,
        None => return Ok(Vec::new()),
    };

    let projects_tree = projects_entry
        .to_object(repo)
        .context("Failed to get projects object")?
        .peel_to_tree()
        .context("Failed to peel to projects tree")?;

    let mut projects = Vec::new();
    for entry in projects_tree.iter() {
        if let Some(name) = entry.name() {
            if entry.kind() == Some(git2::ObjectType::Tree) {
                projects.push(name.to_string());
            }
        }
    }

    projects.sort();
    Ok(projects)
}

/// Get the date of the most recent commit that modified a project
pub fn get_project_updated(repo: &Repository, project: &str) -> Result<Option<NaiveDate>> {
    let path = format!("{}/{}", PROJECTS_DIR, project);
    let mut revwalk = repo.revwalk().context("Failed to create revwalk")?;
    revwalk.push_head().context("Failed to push HEAD")?;

    for oid in revwalk {
        let oid = oid.context("Failed to get commit oid")?;
        let commit = repo.find_commit(oid).context("Failed to find commit")?;
        let tree = commit.tree().context("Failed to get commit tree")?;

        // Get the project subtree OID in this commit (if it exists)
        let current_oid = tree.get_path(Path::new(&path)).ok().map(|entry| entry.id());

        // Compare with parent commit's tree
        let parent_oid = commit
            .parent(0)
            .ok()
            .and_then(|parent| parent.tree().ok())
            .and_then(|parent_tree| parent_tree.get_path(Path::new(&path)).ok())
            .map(|entry| entry.id());

        // If the OIDs differ, this commit modified the project
        if current_oid != parent_oid {
            let time = commit.time();
            let dt =
                chrono::DateTime::from_timestamp(time.seconds(), 0).context("Invalid timestamp")?;
            let local = dt.with_timezone(&chrono::Local);
            return Ok(Some(local.date_naive()));
        }
    }

    Ok(None)
}

/// Fetch commits for a project within a date range.
///
/// Returns commits where:
/// - The commit modified files under `projects/{project}/`
/// - The commit is NOT a journal-only commit (all changes in `*/journal/*`)
/// - The commit timestamp falls within [since, until]
pub fn get_project_commits(
    repo: &Repository,
    project: &str,
    since: Option<NaiveDate>,
    until: Option<NaiveDate>,
) -> Result<Vec<CommitEntry>> {
    let project_path = format!("{}/{}", PROJECTS_DIR, project);
    let journal_path = format!("{}/journal/", project_path);

    let mut revwalk = repo.revwalk().context("Failed to create revwalk")?;
    revwalk.push_head().context("Failed to push HEAD")?;

    let mut commits = Vec::new();

    for oid in revwalk {
        let oid = oid.context("Failed to get commit oid")?;
        let commit = repo.find_commit(oid).context("Failed to find commit")?;

        // Skip merge commits
        if commit.parent_count() > 1 {
            continue;
        }

        // Convert commit time to local datetime
        let time = commit.time();
        let dt =
            chrono::DateTime::from_timestamp(time.seconds(), 0).context("Invalid timestamp")?;
        let local = dt.with_timezone(&Local);
        let date = local.date_naive();

        // Filter by date range - exit early if we've gone past the since date
        if let Some(s) = since {
            if date < s {
                break; // Commits are in chronological order (newest first), so we can stop
            }
        }
        if let Some(u) = until {
            if date > u {
                continue;
            }
        }

        // Get changed paths between this commit and its parent
        let tree = commit.tree().context("Failed to get commit tree")?;
        let parent_tree = commit.parent(0).ok().and_then(|p| p.tree().ok());
        let changed_paths = match get_changed_paths(repo, parent_tree.as_ref(), &tree) {
            Ok(paths) => paths,
            Err(_) => continue, // Skip commits with missing objects (sparse checkout)
        };

        // Filter: must have changes in project path
        let project_changes: Vec<_> = changed_paths
            .iter()
            .filter(|p| p.starts_with(&project_path))
            .collect();

        if project_changes.is_empty() {
            continue;
        }

        // Filter: skip if ALL changes are in journal/
        let all_journal = project_changes.iter().all(|p| p.starts_with(&journal_path));
        if all_journal {
            continue;
        }

        // Build commit entry
        let subject = commit.summary().unwrap_or("").to_string();
        let body = commit.body().map(sanitize_commit_body);
        let short_hash = oid.to_string()[..7].to_string();

        commits.push(CommitEntry {
            datetime: local.naive_local(),
            subject,
            body,
            short_hash,
        });
    }

    Ok(commits)
}

/// Get paths that changed between parent tree and current tree.
fn get_changed_paths(
    repo: &Repository,
    parent_tree: Option<&git2::Tree>,
    tree: &git2::Tree,
) -> Result<Vec<String>> {
    let mut paths = Vec::new();

    let diff = repo
        .diff_tree_to_tree(parent_tree, Some(tree), None)
        .context("Failed to diff trees")?;

    diff.foreach(
        &mut |delta, _| {
            if let Some(path) = delta.new_file().path() {
                paths.push(path.to_string_lossy().to_string());
            }
            true
        },
        None,
        None,
        None,
    )
    .context("Failed to iterate diff")?;

    Ok(paths)
}

/// Sanitize commit body by stripping heading prefixes and generated-by lines.
fn sanitize_commit_body(body: &str) -> String {
    body.lines()
        .filter(|line| {
            let trimmed = line.trim();
            // Skip Claude Code generated-by lines
            !trimmed.starts_with("🤖 Generated with")
                && !trimmed.starts_with("Co-Authored-By: Claude")
        })
        .map(|line| {
            let trimmed = line.trim_start();
            if trimmed.starts_with('#') {
                trimmed.trim_start_matches('#').trim_start().to_string()
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Read a file from HEAD tree (works even if file isn't checked out)
pub fn read_blob(repo: &Repository, path: &str) -> Result<String> {
    let head = repo.head().context("Failed to get HEAD")?;
    let tree = head
        .peel_to_tree()
        .context("Failed to get tree from HEAD")?;
    let entry = tree
        .get_path(Path::new(path))
        .context("Path not found in tree")?;
    let blob = entry
        .to_object(repo)
        .context("Failed to get object")?
        .peel_to_blob()
        .context("Failed to peel to blob")?;
    let content = std::str::from_utf8(blob.content()).context("Invalid UTF-8 in blob")?;
    Ok(content.to_string())
}

/// List all file paths in the HEAD tree (like `git ls-tree -r HEAD --name-only`).
/// Returns paths relative to repo root (e.g., "projects/meow/README.md").
pub fn list_all_paths(repo: &Repository) -> Result<std::collections::HashSet<String>> {
    let head = repo.head().context("Failed to get HEAD")?;
    let tree = head
        .peel_to_tree()
        .context("Failed to get tree from HEAD")?;

    let mut paths = std::collections::HashSet::new();
    tree.walk(git2::TreeWalkMode::PreOrder, |dir, entry| {
        if entry.kind() == Some(git2::ObjectType::Blob) {
            if let Some(name) = entry.name() {
                let full_path = if dir.is_empty() {
                    name.to_string()
                } else {
                    format!("{}{}", dir, name)
                };
                let _ = paths.insert(full_path);
            }
        }
        git2::TreeWalkResult::Ok
    })
    .context("Failed to walk tree")?;

    Ok(paths)
}

/// Detect project name from current working directory.
/// Returns Some(project_name) if CWD is under projects/{name}/.
/// Works for both main repo (~/src/projects/) and worktrees (~/.meow/trees/*/projects/).
pub fn detect_project_from_cwd(root: &Path) -> Result<Option<String>> {
    let cwd = env::current_dir().context("Failed to get current directory")?;

    // Try main repo first
    let projects_dir = root.join(PROJECTS_DIR);
    if let Ok(relative) = cwd.strip_prefix(&projects_dir) {
        if let Some(project) = relative.iter().next() {
            if let Some(name) = project.to_str() {
                return Ok(Some(name.to_string()));
            }
        }
    }

    // Try worktree paths (XDG_DATA_HOME/meow/trees/*/projects/{project})
    if let Ok(worktree_base) = worktree_dir() {
        if let Ok(relative) = cwd.strip_prefix(&worktree_base) {
            // Path is {branch}/projects/{project}/...
            let mut components = relative.iter();
            let _branch = components.next(); // skip branch dir
            let projects = components.next(); // should be "projects"
            let project = components.next(); // project name
            if projects.and_then(|p| p.to_str()) == Some(PROJECTS_DIR) {
                if let Some(name) = project.and_then(|p| p.to_str()) {
                    return Ok(Some(name.to_string()));
                }
            }
        }
    }

    Ok(None)
}

/// Prune stale worktree git metadata (worktrees whose directories no longer exist).
pub fn prune_worktrees(repo_root: &Path) -> Result<()> {
    let _ = Command::new("git")
        .args(["worktree", "prune"])
        .current_dir(repo_root)
        .output()
        .context("Failed to prune worktrees")?;
    Ok(())
}

/// Check if a worktree has uncommitted changes.
pub fn is_worktree_dirty(worktree_path: &Path) -> bool {
    Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(worktree_path)
        .output()
        .map(|output| !output.stdout.is_empty())
        .unwrap_or(false)
}

/// Count worktrees older than the given duration.
pub fn count_old_worktrees(max_age: std::time::Duration) -> usize {
    let trees_dir = match worktree_dir() {
        Ok(d) => d,
        Err(_) => return 0,
    };
    if !trees_dir.exists() {
        return 0;
    }

    let cutoff = std::time::SystemTime::now() - max_age;
    std::fs::read_dir(&trees_dir)
        .into_iter()
        .flatten()
        .flatten()
        .filter(|entry| {
            entry
                .metadata()
                .and_then(|m| m.modified())
                .map(|mtime| mtime < cutoff)
                .unwrap_or(false)
        })
        .count()
}

/// Find the worktree path for a given branch, if one exists.
fn find_worktree_for_branch(repo_root: &Path, branch_name: &str) -> Option<PathBuf> {
    let output = Command::new("git")
        .args(["worktree", "list", "--porcelain"])
        .current_dir(repo_root)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut current_worktree: Option<PathBuf> = None;

    for line in stdout.lines() {
        if let Some(path) = line.strip_prefix("worktree ") {
            current_worktree = Some(PathBuf::from(path));
        } else if let Some(branch) = line.strip_prefix("branch refs/heads/") {
            if branch == branch_name {
                return current_worktree;
            }
        } else if line.is_empty() {
            current_worktree = None;
        }
    }

    None
}

/// Create a fresh git worktree for a project, or reuse an existing one.
/// Returns the path to the project subdirectory within the worktree.
/// If `branch` is provided, uses that as the branch name; otherwise generates a timestamped name.
/// If the branch already exists, reuses it and attempts to merge origin/main.
pub fn create_worktree(repo_root: &Path, project: &str, branch: Option<&str>) -> Result<PathBuf> {
    // Prune stale worktrees first
    prune_worktrees(repo_root)?;

    // Warn about old worktrees
    let old_count = count_old_worktrees(std::time::Duration::from_secs(24 * 60 * 60));
    if old_count > 0 {
        tracing::warn!(
            "{} worktree(s) older than 24h - consider running 'meow prune'",
            old_count
        );
    }

    // Generate worktree and branch names
    // If branch is provided, use it for both; otherwise use timestamp
    let timestamp = Local::now().format("%Y%m%d-%H%M%S");
    let worktree_name = match branch {
        Some(b) => b.to_string(),
        None => format!("{}-{}", project, timestamp),
    };
    let branch_name = branch.unwrap_or(&worktree_name);

    // Worktree location: XDG_DATA_HOME/meow/trees/{branch|project-timestamp}
    // Check if branch already has a worktree somewhere (possibly with old naming convention)
    let worktree_root = if let Some(existing) = find_worktree_for_branch(repo_root, branch_name) {
        tracing::info!(
            "Branch '{}' already has worktree at {:?}",
            branch_name,
            existing
        );
        existing
    } else {
        worktree_dir()?.join(&worktree_name)
    };

    // Check if worktree already exists
    let worktree_exists = worktree_root.exists();

    if !worktree_exists {
        // Create parent directory if needed
        if let Some(parent) = worktree_root.parent() {
            std::fs::create_dir_all(parent)
                .context("Failed to create worktree parent directory")?;
        }

        // Check if the branch already exists
        let branch_exists = Command::new("git")
            .args(["rev-parse", "--verify", branch_name])
            .current_dir(repo_root)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false);

        // Create the worktree - with new branch or using existing one
        let output = if branch_exists {
            tracing::info!("Reusing existing branch '{}'", branch_name);
            Command::new("git")
                .args([
                    "worktree",
                    "add",
                    &worktree_root.to_string_lossy(),
                    branch_name,
                ])
                .current_dir(repo_root)
                .output()
                .context("Failed to create worktree")?
        } else {
            Command::new("git")
                .args([
                    "worktree",
                    "add",
                    "-b",
                    branch_name,
                    &worktree_root.to_string_lossy(),
                    "origin/main",
                ])
                .current_dir(repo_root)
                .output()
                .context("Failed to create worktree")?
        };

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            anyhow::bail!("git worktree add failed: {}", stderr);
        }
    } else {
        tracing::info!("Reusing existing worktree at {:?}", worktree_root);
    }

    // Try to merge origin/main (should be up to date after shipit push)
    let merge_output = Command::new("git")
        .args(["merge", "origin/main", "--no-edit"])
        .current_dir(&worktree_root)
        .output();

    if let Ok(output) = merge_output {
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("CONFLICT") || stderr.contains("Automatic merge failed") {
                // Abort the merge so the worktree is usable
                let _ = Command::new("git")
                    .args(["merge", "--abort"])
                    .current_dir(&worktree_root)
                    .output();
                tracing::warn!("Merge conflicts with origin/main - branch not updated");
            }
        }
    }

    // Add trust entry to ~/.claude.json so Claude Code doesn't prompt
    let home = dirs::home_dir().context("Failed to get home directory")?;
    let claude_config = home.join(".claude.json");
    if claude_config.exists() {
        if let Ok(content) = std::fs::read_to_string(&claude_config) {
            if let Ok(mut json) = serde_json::from_str::<serde_json::Value>(&content) {
                if let Some(projects) = json.get_mut("projects").and_then(|p| p.as_object_mut()) {
                    let worktree_key = worktree_root.to_string_lossy().to_string();
                    let _ = projects.insert(
                        worktree_key,
                        serde_json::json!({
                            "hasTrustDialogAccepted": true
                        }),
                    );
                    let _ = std::fs::write(
                        &claude_config,
                        serde_json::to_string_pretty(&json).unwrap_or_default(),
                    );
                }
            }
        }
    }

    // Path to project subdirectory within worktree
    let project_path = worktree_root.join(PROJECTS_DIR).join(project);

    // Copy Claude settings from original project if they exist
    let original_project = repo_root
        .join(PROJECTS_DIR)
        .join(project)
        .join(".claude")
        .join("settings.local.json");
    if original_project.exists() {
        let wt_claude_dir = project_path.join(".claude");
        let _ = std::fs::create_dir_all(&wt_claude_dir);
        let _ = std::fs::copy(&original_project, wt_claude_dir.join("settings.local.json"));
    }

    // Allow direnv for the project's .envrc
    let _ = Command::new("direnv")
        .args(["allow"])
        .current_dir(&project_path)
        .output();

    Ok(project_path)
}

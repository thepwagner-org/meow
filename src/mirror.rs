//! GitHub mirroring functionality.
//!
//! Copies filtered project content to a local clone of a GitHub repository
//! for review and manual push.

use crate::config;
use crate::github;
use crate::markdown::{parse, Frontmatter};
use crate::PROJECTS_DIR;
use anyhow::{bail, Context, Result};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command;

/// Hardcoded paths that are always excluded from mirrors.
/// These are git-tracked files that should remain private.
/// Build artifacts (target/, node_modules/, etc.) are already excluded
/// because we use `git ls-files` to enumerate source files.
pub const HARDCODED_EXCLUDES: &[&str] = &[
    "journal/",
    ".meow.d/",
    "AGENTS.md",
    "CLAUDE.md",
    ".claude/",
    ".opencode/",
    ".envrc",
];

/// MIT license text to inject into mirrored repositories.
const MIT_LICENSE: &str = include_str!("templates/mit-license.md");

/// A workflow template.
struct WorkflowTemplate {
    name: &'static str,
    workflow: &'static str,
}

/// Known workflow templates, keyed by name used in frontmatter `github-ci` list.
const WORKFLOW_TEMPLATES: &[WorkflowTemplate] = &[
    WorkflowTemplate {
        name: "cargo-ci",
        workflow: include_str!("templates/cargo-ci.yaml"),
    },
    WorkflowTemplate {
        name: "cargo-release",
        workflow: include_str!("templates/cargo-release.yaml"),
    },
    WorkflowTemplate {
        name: "pnpm-ci",
        workflow: include_str!("templates/pnpm-ci.yaml"),
    },
];

/// Mirror configuration extracted from README frontmatter.
#[derive(Debug, Clone)]
pub struct MirrorConfig {
    /// GitHub org name
    pub org: String,
    /// GitHub repo name
    pub repo: String,
    /// Additional paths to exclude
    pub exclude: Vec<String>,
    /// Workflow templates to inject (e.g., ["cargo-ci", "pnpm-ci"])
    pub workflows: Vec<String>,
}

impl MirrorConfig {
    /// Extract mirror config from README frontmatter.
    /// Returns None if `github` field is not present.
    pub fn from_frontmatter(fm: &Frontmatter) -> Option<Self> {
        // Get github field (e.g., "thepwagner-org/rufio-ts")
        let github = fm.extra.get("github")?.as_str()?;
        let (org, repo) = github.split_once('/')?;

        // Get optional github-exclude list
        let exclude = fm
            .extra
            .get("github-exclude")
            .and_then(|v| v.as_sequence())
            .map(|seq| {
                seq.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        // Get optional github-ci workflow list
        let workflows = fm
            .extra
            .get("github-ci")
            .and_then(|v| v.as_sequence())
            .map(|seq| {
                seq.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        Some(Self {
            org: org.to_string(),
            repo: repo.to_string(),
            exclude,
            workflows,
        })
    }

    /// Validate that all configured workflow names are known templates.
    /// Returns an error listing unknown names.
    pub fn validate_workflows(&self) -> Result<()> {
        let known: HashSet<&str> = WORKFLOW_TEMPLATES.iter().map(|t| t.name).collect();
        let unknown: Vec<&str> = self
            .workflows
            .iter()
            .filter(|w| !known.contains(w.as_str()))
            .map(|w| w.as_str())
            .collect();

        if !unknown.is_empty() {
            let valid: Vec<&str> = WORKFLOW_TEMPLATES.iter().map(|t| t.name).collect();
            bail!(
                "Unknown workflow template(s): {}. Valid options: {}",
                unknown.join(", "),
                valid.join(", ")
            );
        }
        Ok(())
    }
}

/// Returns the base directory for mirror clones.
/// Uses $XDG_DATA_HOME/meow/mirrors/ or ~/.local/share/meow/mirrors/
pub fn mirrors_dir() -> Result<PathBuf> {
    Ok(config::data_dir()?.join("mirrors"))
}

/// Get the path where a mirror clone should live.
pub fn mirror_path(org: &str, repo: &str) -> Result<PathBuf> {
    Ok(mirrors_dir()?.join(org).join(repo))
}

/// Parsed content of the `monorepo-synced` tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyncInfo {
    /// Monorepo commit hash that was last synced.
    pub commit: String,
    /// Content hash of the workflow templates that were injected.
    pub templates_hash: String,
}

impl SyncInfo {
    /// Format as tag message content (v1 format).
    pub fn to_tag_message(&self) -> String {
        format!(
            "v1\ncommit:{}\ntemplates:{}",
            self.commit, self.templates_hash
        )
    }

    /// Parse from tag message content. Returns None if unparseable.
    ///
    /// Supports two formats:
    /// - v1: structured `v1\ncommit:<hash>\ntemplates:<hash>`
    /// - legacy: bare commit hash (40-char hex string)
    pub fn from_tag_message(msg: &str) -> Option<Self> {
        let msg = msg.trim();
        let mut lines = msg.lines();
        let first_line = lines.next()?;

        if first_line == "v1" {
            let mut commit = None;
            let mut templates_hash = None;
            for line in lines {
                if let Some(value) = line.strip_prefix("commit:") {
                    commit = Some(value.to_string());
                } else if let Some(value) = line.strip_prefix("templates:") {
                    templates_hash = Some(value.to_string());
                }
            }
            Some(Self {
                commit: commit?,
                templates_hash: templates_hash?,
            })
        } else if first_line.len() == 40 && first_line.chars().all(|c| c.is_ascii_hexdigit()) {
            // Legacy format: bare commit hash, no template tracking
            Some(Self {
                commit: first_line.to_string(),
                templates_hash: String::new(),
            })
        } else {
            None
        }
    }
}

/// Compute a content hash for the given workflow template names.
/// The hash changes when template contents change or the set of workflows changes.
pub fn compute_template_hash(workflows: &[String]) -> String {
    let mut hasher = DefaultHasher::new();

    // Sort to make hash independent of frontmatter ordering
    let mut sorted: Vec<&str> = workflows.iter().map(|s| s.as_str()).collect();
    sorted.sort();

    for name in &sorted {
        name.hash(&mut hasher);
        if let Some(t) = WORKFLOW_TEMPLATES.iter().find(|t| t.name == *name) {
            t.workflow.hash(&mut hasher);
        }
    }

    format!("{:016x}", hasher.finish())
}

/// Ensure a GitHub repo is cloned locally. Creates or fetches as needed.
/// After fetching, resets to origin/main to ensure we can fast-forward push.
pub fn ensure_clone(org: &str, repo: &str) -> Result<PathBuf> {
    let path = mirror_path(org, repo)?;
    let github_url = format!("git@github.com:{}/{}.git", org, repo);

    if path.exists() {
        // Fetch latest
        tracing::debug!(path = %path.display(), "fetching existing mirror");
        let output = Command::new("git")
            .args(["fetch", "--all", "--prune"])
            .current_dir(&path)
            .output()
            .context("failed to run git fetch")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            tracing::warn!("git fetch warning: {}", stderr.trim());
        }

        // Reset to origin/main to ensure our next commit is a fast-forward
        tracing::debug!(path = %path.display(), "resetting to origin/main");
        let output = Command::new("git")
            .args(["reset", "--hard", "origin/main"])
            .current_dir(&path)
            .output()
            .context("failed to run git reset")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            tracing::warn!("git reset warning: {}", stderr.trim());
        }
    } else {
        // Clone
        tracing::info!(url = %github_url, path = %path.display(), "cloning mirror");
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).context("failed to create mirrors directory")?;
        }

        let output = Command::new("git")
            .args(["clone", &github_url, &path.to_string_lossy()])
            .output()
            .context("failed to run git clone")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("git clone failed: {}", stderr.trim());
        }
    }

    Ok(path)
}

/// Build the set of exclude patterns (hardcoded + per-project).
fn build_excludes(config: &MirrorConfig) -> HashSet<String> {
    let mut excludes: HashSet<String> = HARDCODED_EXCLUDES.iter().map(|s| s.to_string()).collect();

    for pattern in &config.exclude {
        let _ = excludes.insert(pattern.clone());
    }

    excludes
}

/// Check if a path should be excluded based on exclude patterns.
fn should_exclude(path: &Path, excludes: &HashSet<String>) -> bool {
    let path_str = path.to_string_lossy();

    for pattern in excludes {
        if pattern.ends_with('/') {
            // Directory pattern
            let dir_name = pattern.trim_end_matches('/');
            // Check if any component matches
            for component in path.components() {
                if let std::path::Component::Normal(name) = component {
                    if name.to_string_lossy() == dir_name {
                        return true;
                    }
                }
            }
        } else if pattern.contains('*') {
            // Glob-like pattern (e.g., "*.local.*")
            // Convert to simple contains check for the non-wildcard part
            let parts: Vec<&str> = pattern.split('*').filter(|s| !s.is_empty()).collect();
            let matches = parts.iter().all(|part| path_str.contains(part));
            if matches {
                return true;
            }
        } else {
            // Exact filename match
            if let Some(name) = path.file_name() {
                if name.to_string_lossy() == *pattern {
                    return true;
                }
            }
        }
    }

    false
}

/// Strip YAML frontmatter from content if present.
fn strip_frontmatter(content: &str) -> String {
    if !content.starts_with("---\n") && !content.starts_with("---\r\n") {
        return content.to_string();
    }

    let search_start = if content.starts_with("---\r\n") { 5 } else { 4 };
    if let Some(end_pos) = content[search_start..].find("\n---") {
        let after_frontmatter = search_start + end_pos + 4; // "\n---"
        let rest = &content[after_frontmatter..];
        rest.strip_prefix('\n')
            .or_else(|| rest.strip_prefix("\r\n"))
            .unwrap_or(rest)
            .to_string()
    } else {
        content.to_string()
    }
}

/// Get list of git-tracked files in a project directory.
fn get_tracked_files(project_path: &Path) -> Result<Vec<PathBuf>> {
    let output = Command::new("git")
        .args(["ls-files", "--cached"])
        .current_dir(project_path)
        .output()
        .context("Failed to run git ls-files")?;

    if !output.status.success() {
        anyhow::bail!(
            "git ls-files failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let files = String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(PathBuf::from)
        .collect();

    Ok(files)
}

/// Clean the mirror directory and copy filtered files from source project.
pub fn sync_to_mirror(
    source_project: &Path,
    mirror_path: &Path,
    config: &MirrorConfig,
) -> Result<usize> {
    let excludes = build_excludes(config);

    // Clean slate: remove all tracked files in mirror (keep .git)
    clean_mirror(mirror_path)?;

    // Get git-tracked files and copy those that aren't excluded
    let tracked_files = get_tracked_files(source_project)?;
    let mut file_count = 0;

    for relative_path in tracked_files {
        if should_exclude(&relative_path, &excludes) {
            tracing::debug!(path = %relative_path.display(), "excluding from mirror");
            continue;
        }

        let source_path = source_project.join(&relative_path);
        let dest_path = mirror_path.join(&relative_path);

        // Create parent directories as needed
        if let Some(parent) = dest_path.parent() {
            fs::create_dir_all(parent)?;
        }

        // Copy the file
        let _ = fs::copy(&source_path, &dest_path)?;
        file_count += 1;
    }

    // Transform README.md (strip frontmatter)
    let readme_path = mirror_path.join("README.md");
    if readme_path.exists() {
        let content = fs::read_to_string(&readme_path)?;
        let stripped = strip_frontmatter(&content);
        if stripped.len() != content.len() {
            fs::write(&readme_path, stripped)?;
        }
    }

    // Inject LICENSE.md
    fs::write(mirror_path.join("LICENSE.md"), MIT_LICENSE)?;
    file_count += 1;

    // Inject workflow templates
    if !config.workflows.is_empty() {
        let workflows_dir = mirror_path.join(".github").join("workflows");
        fs::create_dir_all(&workflows_dir)?;

        for workflow_name in &config.workflows {
            if let Some(template) = WORKFLOW_TEMPLATES
                .iter()
                .find(|t| t.name == workflow_name.as_str())
            {
                let filename = format!("{}.yaml", workflow_name);
                fs::write(workflows_dir.join(&filename), template.workflow)?;
                file_count += 1;

                tracing::debug!(workflow = %workflow_name, "injected workflow template");
            }
        }
    }

    Ok(file_count)
}

/// Remove all files in the mirror except .git directory.
fn clean_mirror(mirror_path: &Path) -> Result<()> {
    for entry in fs::read_dir(mirror_path)? {
        let entry = entry?;
        let name = entry.file_name();
        if name == ".git" {
            continue;
        }

        let path = entry.path();
        if path.is_dir() {
            fs::remove_dir_all(&path)?;
        } else {
            fs::remove_file(&path)?;
        }
    }
    Ok(())
}

/// Get mirror status: returns true if mirror has uncommitted changes.
pub fn has_uncommitted_changes(mirror_path: &Path) -> bool {
    Command::new("git")
        .args(["status", "--porcelain"])
        .current_dir(mirror_path)
        .output()
        .map(|output| !output.stdout.is_empty())
        .unwrap_or(false)
}

/// Get mirror status: returns true if mirror has unpushed commits.
pub fn has_unpushed_commits(mirror_path: &Path) -> bool {
    Command::new("git")
        .args(["log", "@{upstream}..HEAD", "--oneline"])
        .current_dir(mirror_path)
        .output()
        .map(|output| !output.stdout.is_empty())
        .unwrap_or(false)
}

/// Check if the project directory has uncommitted changes in the monorepo.
pub fn is_project_dirty(root: &Path, project: &str) -> bool {
    let project_path = root.join(PROJECTS_DIR).join(project);
    Command::new("git")
        .args(["status", "--porcelain", "--"])
        .arg(&project_path)
        .current_dir(root)
        .output()
        .map(|output| !output.stdout.is_empty())
        .unwrap_or(false)
}

/// Read the sync info from the mirror's `monorepo-synced` tag.
/// Returns None if the tag doesn't exist or can't be parsed.
pub fn get_sync_info(mirror_path: &Path) -> Option<SyncInfo> {
    let msg = Command::new("git")
        .args(["tag", "-l", "--format=%(contents)", "monorepo-synced"])
        .current_dir(mirror_path)
        .output()
        .ok()
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
        .filter(|s| !s.is_empty())?;

    SyncInfo::from_tag_message(&msg)
}

/// Write sync info to `.git/meow-sync-info` in the mirror clone.
/// The `/shipit-mirror` command reads this file for the tag message.
pub fn write_sync_info(mirror_path: &Path, info: &SyncInfo) -> Result<()> {
    let path = mirror_path.join(".git").join("meow-sync-info");
    fs::write(&path, info.to_tag_message()).context("Failed to write meow-sync-info")?;
    tracing::debug!(path = %path.display(), "wrote sync info");
    Ok(())
}

/// Get the latest monorepo commit that touched the project directory.
pub fn get_latest_project_commit(root: &Path, project: &str) -> Result<String> {
    let project_path = root.join(PROJECTS_DIR).join(project);
    let output = Command::new("git")
        .args(["log", "-1", "--format=%H", "--"])
        .arg(&project_path)
        .current_dir(root)
        .output()
        .context("Failed to run git log")?;

    let hash = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if hash.is_empty() {
        bail!("No commits found for project '{}'", project);
    }
    Ok(hash)
}

/// Count commits between two commits that touched a project directory.
/// Returns the number of commits in `from..to` range that modified the project.
fn count_project_commits(root: &Path, project: &str, from: &str, to: &str) -> Option<usize> {
    let project_path = root.join(PROJECTS_DIR).join(project);
    let range = format!("{}..{}", from, to);
    let output = Command::new("git")
        .args(["rev-list", "--count", &range, "--"])
        .arg(&project_path)
        .current_dir(root)
        .output()
        .ok()?;

    String::from_utf8_lossy(&output.stdout).trim().parse().ok()
}

/// Find all projects that have mirror configuration.
pub fn find_mirrored_projects(root: &Path) -> Result<Vec<(String, MirrorConfig)>> {
    let projects_dir = root.join(PROJECTS_DIR);
    let mut results = Vec::new();

    if !projects_dir.exists() {
        return Ok(results);
    }

    for entry in fs::read_dir(&projects_dir)? {
        let entry = entry?;
        if !entry.file_type()?.is_dir() {
            continue;
        }

        let project_name = entry.file_name().to_string_lossy().to_string();
        let readme_path = entry.path().join("README.md");

        if !readme_path.exists() {
            continue;
        }

        if let Ok(content) = fs::read_to_string(&readme_path) {
            let doc = parse(&content);
            if let Some(ref fm) = doc.frontmatter {
                if let Some(config) = MirrorConfig::from_frontmatter(fm) {
                    results.push((project_name, config));
                }
            }
        }
    }

    results.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(results)
}

/// Result of checking/creating a version tag on the mirror remote.
#[derive(Debug, PartialEq, Eq)]
pub enum VersionTagResult {
    /// Tag was created and pushed.
    Created(String),
    /// Tag already existed on the remote.
    AlreadyExists(String),
}

/// Read the version string from a project's `version.toml`.
/// Returns None if the file doesn't exist or can't be parsed.
pub fn read_project_version(root: &Path, project: &str) -> Option<String> {
    let path = root.join(PROJECTS_DIR).join(project).join("version.toml");
    let content = fs::read_to_string(path).ok()?;
    parse_version_toml(&content)
}

/// Parse a version string from version.toml content.
/// Expects a line like `version = "1.2.3"`.
fn parse_version_toml(content: &str) -> Option<String> {
    for line in content.lines() {
        let line = line.trim();
        if let Some(rest) = line.strip_prefix("version") {
            let rest = rest.trim();
            if let Some(rest) = rest.strip_prefix('=') {
                let rest = rest.trim();
                let rest = rest.strip_prefix('"')?.strip_suffix('"')?;
                if !rest.is_empty() {
                    return Some(rest.to_string());
                }
            }
        }
    }
    None
}

/// Check if a version tag exists on the mirror remote, create and push it if not.
pub fn tag_version_if_needed(mirror_path: &Path, version: &str) -> Result<VersionTagResult> {
    let tag_name = format!("v{}", version);

    // Check if tag exists on the remote
    let output = Command::new("git")
        .args([
            "ls-remote",
            "--tags",
            "origin",
            &format!("refs/tags/{}", tag_name),
        ])
        .current_dir(mirror_path)
        .output()
        .context("failed to run git ls-remote")?;

    let stdout = String::from_utf8_lossy(&output.stdout);
    if !stdout.trim().is_empty() {
        return Ok(VersionTagResult::AlreadyExists(tag_name));
    }

    // Create annotated tag
    let output = Command::new("git")
        .args(["tag", "-a", &tag_name, "-m", &tag_name])
        .current_dir(mirror_path)
        .output()
        .context("failed to create version tag")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git tag failed: {}", stderr.trim());
    }

    // Push the tag
    let output = Command::new("git")
        .args(["push", "origin", &tag_name])
        .current_dir(mirror_path)
        .output()
        .context("failed to push version tag")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git push tag failed: {}", stderr.trim());
    }

    Ok(VersionTagResult::Created(tag_name))
}

/// Commit staged changes, push to origin/main, and update tracking tag.
pub fn push_mirror(mirror_path: &Path, message: &str) -> Result<PushResult> {
    let mut result = PushResult::default();

    // Stage all changes
    let output = Command::new("git")
        .args(["add", "-A"])
        .current_dir(mirror_path)
        .output()
        .context("failed to run git add")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git add failed: {}", stderr.trim());
    }

    // Commit (bail if nothing to commit)
    let output = Command::new("git")
        .args(["commit", "-m", message])
        .current_dir(mirror_path)
        .output()
        .context("failed to run git commit")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout_str = String::from_utf8_lossy(&output.stdout);
        if stdout_str.contains("nothing to commit") || stderr.contains("nothing to commit") {
            bail!("Nothing to commit in mirror");
        }
        bail!("git commit failed: {}", stderr.trim());
    }

    // Extract short hash from commit
    let commit_hash = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .current_dir(mirror_path)
        .output()
        .ok()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default();
    result.commit_hash = Some(commit_hash);

    // Push to origin main
    let output = Command::new("git")
        .args(["push", "origin", "main"])
        .current_dir(mirror_path)
        .output()
        .context("failed to run git push")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!(
            "git push failed (committed as {}): {}",
            result.commit_hash.as_deref().unwrap_or("?"),
            stderr.trim()
        );
    }
    result.pushed = true;

    // Update monorepo-synced tracking tag from .git/meow-sync-info
    let sync_info_path = mirror_path.join(".git").join("meow-sync-info");
    if sync_info_path.exists() {
        let tag_message =
            fs::read_to_string(&sync_info_path).context("failed to read .git/meow-sync-info")?;

        let tag_ok = Command::new("git")
            .args(["tag", "-f", "-a", "monorepo-synced", "-m", &tag_message])
            .current_dir(mirror_path)
            .output()
            .context("failed to create tracking tag")?;

        if !tag_ok.status.success() {
            let stderr = String::from_utf8_lossy(&tag_ok.stderr);
            tracing::warn!("tracking tag failed: {}", stderr.trim());
            result.tracking_tag_error = Some(stderr.trim().to_string());
        } else {
            let push_ok = Command::new("git")
                .args(["push", "origin", "monorepo-synced", "--force"])
                .current_dir(mirror_path)
                .output()
                .context("failed to push tracking tag")?;

            if !push_ok.status.success() {
                let stderr = String::from_utf8_lossy(&push_ok.stderr);
                tracing::warn!("tracking tag push failed: {}", stderr.trim());
                result.tracking_tag_error = Some(stderr.trim().to_string());
            } else {
                result.tracking_tag_updated = true;
            }
        }
    } else {
        tracing::warn!("no .git/meow-sync-info found — skipping tracking tag");
    }

    Ok(result)
}

/// Update the GitHub repo description to match README frontmatter.
pub fn sync_repo_description(org: &str, repo: &str, description: &str) -> Result<()> {
    let repo_slug = format!("{org}/{repo}");
    let output = Command::new("gh")
        .args(["repo", "edit", &repo_slug, "--description", description])
        .output()
        .context("failed to run gh repo edit")?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("gh repo edit failed: {}", stderr.trim());
    }
    Ok(())
}

/// Summary of what `push_mirror` accomplished.
#[derive(Debug, Default)]
pub struct PushResult {
    /// Short commit hash, if commit succeeded.
    pub commit_hash: Option<String>,
    /// Whether the push to origin/main succeeded.
    pub pushed: bool,
    /// Whether the monorepo-synced tracking tag was updated.
    pub tracking_tag_updated: bool,
    /// Error message if tracking tag update failed (partial success).
    pub tracking_tag_error: Option<String>,
}

/// Mirror status for a single project.
#[derive(Debug)]
pub struct MirrorStatus {
    pub project: String,
    pub config: MirrorConfig,
    /// Parsed sync info from mirror tag (None if mirror doesn't exist or no tag)
    pub sync_info: Option<SyncInfo>,
    /// Latest monorepo commit touching this project
    pub private_commit: String,
    /// Number of commits between public and private (None if public unknown)
    pub commits_ahead: Option<usize>,
    /// Whether the workflow templates are stale (current hash doesn't match synced hash)
    pub templates_stale: bool,
    /// Open Dependabot alerts on the public repo (None if `gh` unavailable)
    pub dependabot_alerts: Option<Vec<github::DependabotAlert>>,
    /// Version from version.toml (None if file doesn't exist)
    pub version: Option<String>,
}

/// Get status for all mirrored projects.
pub fn get_all_status(root: &Path) -> Result<Vec<MirrorStatus>> {
    let projects = find_mirrored_projects(root)?;

    // Batch-fetch dependabot alerts for all repos in one GraphQL query
    let repos: Vec<(String, String)> = projects
        .iter()
        .map(|(_, c)| (c.org.clone(), c.repo.clone()))
        .collect();
    let alerts_map = github::fetch_dependabot_alerts(&repos);

    let mut statuses = Vec::new();
    for (project, config) in projects {
        let mirror = mirror_path(&config.org, &config.repo)?;
        let sync_info = if mirror.exists() {
            get_sync_info(&mirror)
        } else {
            None
        };
        let private_commit = get_latest_project_commit(root, &project)?;
        let commits_ahead = sync_info
            .as_ref()
            .and_then(|si| count_project_commits(root, &project, &si.commit, &private_commit));

        // Check template staleness (skip if project has no templates)
        let templates_stale = if config.workflows.is_empty() {
            false
        } else {
            let expected_hash = compute_template_hash(&config.workflows);
            sync_info
                .as_ref()
                .map(|si| si.templates_hash != expected_hash)
                .unwrap_or(false)
        };

        let dependabot_alerts = alerts_map
            .as_ref()
            .and_then(|m| m.get(&(config.org.clone(), config.repo.clone())))
            .cloned();

        let version = read_project_version(root, &project);

        statuses.push(MirrorStatus {
            project,
            config: config.clone(),
            sync_info,
            private_commit,
            commits_ahead,
            templates_stale,
            dependabot_alerts,
            version,
        });
    }

    Ok(statuses)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_should_exclude_directory() {
        let excludes: HashSet<String> = ["journal/", ".claude/"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        assert!(should_exclude(Path::new("journal/2024-01.md"), &excludes));
        assert!(should_exclude(
            Path::new(".claude/settings.json"),
            &excludes
        ));
        assert!(!should_exclude(Path::new("src/main.rs"), &excludes));
    }

    #[test]
    fn test_should_exclude_file() {
        let excludes: HashSet<String> = [".envrc", "CLAUDE.md"]
            .iter()
            .map(|s| s.to_string())
            .collect();

        assert!(should_exclude(Path::new(".envrc"), &excludes));
        assert!(should_exclude(Path::new("CLAUDE.md"), &excludes));
        assert!(!should_exclude(Path::new("README.md"), &excludes));
    }

    #[test]
    fn test_should_exclude_extension_pattern() {
        let excludes: HashSet<String> = ["*.local.*"].iter().map(|s| s.to_string()).collect();

        assert!(should_exclude(Path::new("config.local.json"), &excludes));
        assert!(should_exclude(Path::new("settings.local.yaml"), &excludes));
        assert!(!should_exclude(Path::new("config.json"), &excludes));
    }

    #[test]
    fn test_strip_frontmatter() {
        let content = "---\ncreated: 2025-01-01\ngithub: org/repo\n---\n# Hello\nWorld";
        assert_eq!(strip_frontmatter(content), "# Hello\nWorld");

        let no_fm = "# Hello\nWorld";
        assert_eq!(strip_frontmatter(no_fm), "# Hello\nWorld");

        let unclosed = "---\nfoo: bar\n# Hello";
        assert_eq!(strip_frontmatter(unclosed), "---\nfoo: bar\n# Hello");
    }

    #[test]
    fn test_mirror_config_from_frontmatter() {
        use std::collections::HashMap;

        let mut extra = HashMap::new();
        let _ = extra.insert(
            "github".to_string(),
            serde_yaml::Value::String("thepwagner-org/rufio-ts".to_string()),
        );

        let fm = Frontmatter {
            extra,
            ..Default::default()
        };

        let config = MirrorConfig::from_frontmatter(&fm).unwrap();
        assert_eq!(config.org, "thepwagner-org");
        assert_eq!(config.repo, "rufio-ts");
        assert!(config.exclude.is_empty());
        assert!(config.workflows.is_empty());
    }

    #[test]
    fn test_mirror_config_with_excludes() {
        use std::collections::HashMap;

        let mut extra = HashMap::new();
        let _ = extra.insert(
            "github".to_string(),
            serde_yaml::Value::String("org/repo".to_string()),
        );
        let _ = extra.insert(
            "github-exclude".to_string(),
            serde_yaml::Value::Sequence(vec![
                serde_yaml::Value::String("secrets/".to_string()),
                serde_yaml::Value::String("*.local.*".to_string()),
            ]),
        );

        let fm = Frontmatter {
            extra,
            ..Default::default()
        };

        let config = MirrorConfig::from_frontmatter(&fm).unwrap();
        assert_eq!(config.exclude, vec!["secrets/", "*.local.*"]);
    }

    #[test]
    fn test_mirror_config_with_workflows() {
        use std::collections::HashMap;

        let mut extra = HashMap::new();
        let _ = extra.insert(
            "github".to_string(),
            serde_yaml::Value::String("org/repo".to_string()),
        );
        let _ = extra.insert(
            "github-ci".to_string(),
            serde_yaml::Value::Sequence(vec![serde_yaml::Value::String("cargo-ci".to_string())]),
        );

        let fm = Frontmatter {
            extra,
            ..Default::default()
        };

        let config = MirrorConfig::from_frontmatter(&fm).unwrap();
        assert_eq!(config.workflows, vec!["cargo-ci"]);
    }

    #[test]
    fn test_validate_workflows_known() {
        let config = MirrorConfig {
            org: "org".to_string(),
            repo: "repo".to_string(),
            exclude: vec![],
            workflows: vec!["cargo-ci".to_string()],
        };
        assert!(config.validate_workflows().is_ok());
    }

    #[test]
    fn test_validate_workflows_unknown() {
        let config = MirrorConfig {
            org: "org".to_string(),
            repo: "repo".to_string(),
            exclude: vec![],
            workflows: vec!["unknown-ci".to_string()],
        };
        let err = config.validate_workflows().unwrap_err();
        assert!(err.to_string().contains("unknown-ci"));
        assert!(err.to_string().contains("cargo-ci"));
    }

    #[test]
    fn test_sync_info_roundtrip() {
        let info = SyncInfo {
            commit: "abc123def456".to_string(),
            templates_hash: "7f3a1b2c4d5e0000".to_string(),
        };
        let msg = info.to_tag_message();
        let parsed = SyncInfo::from_tag_message(&msg).unwrap();
        assert_eq!(info, parsed);
    }

    #[test]
    fn test_sync_info_v1_format() {
        let info = SyncInfo {
            commit: "abc123".to_string(),
            templates_hash: "def456".to_string(),
        };
        let msg = info.to_tag_message();
        assert_eq!(msg, "v1\ncommit:abc123\ntemplates:def456");
    }

    #[test]
    fn test_sync_info_rejects_unknown_version() {
        assert!(SyncInfo::from_tag_message("v2\ncommit:abc\ntemplates:def").is_none());
    }

    #[test]
    fn test_sync_info_rejects_missing_fields() {
        assert!(SyncInfo::from_tag_message("v1\ncommit:abc").is_none());
        assert!(SyncInfo::from_tag_message("v1\ntemplates:def").is_none());
    }

    #[test]
    fn test_sync_info_legacy_bare_hash() {
        let info = SyncInfo::from_tag_message("e7efbbf7585c94b505927b8bb29c2067d84c706e").unwrap();
        assert_eq!(info.commit, "e7efbbf7585c94b505927b8bb29c2067d84c706e");
        assert_eq!(info.templates_hash, "");
    }

    #[test]
    fn test_sync_info_rejects_short_hash() {
        assert!(SyncInfo::from_tag_message("abc123").is_none());
    }

    #[test]
    fn test_template_hash_deterministic() {
        let workflows = vec!["cargo-ci".to_string()];
        let h1 = compute_template_hash(&workflows);
        let h2 = compute_template_hash(&workflows);
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_template_hash_order_independent() {
        let a = vec!["cargo-ci".to_string(), "pnpm-ci".to_string()];
        let b = vec!["pnpm-ci".to_string(), "cargo-ci".to_string()];
        assert_eq!(compute_template_hash(&a), compute_template_hash(&b));
    }

    #[test]
    fn test_template_hash_differs_by_set() {
        let cargo = vec!["cargo-ci".to_string()];
        let pnpm = vec!["pnpm-ci".to_string()];
        let both = vec!["cargo-ci".to_string(), "pnpm-ci".to_string()];
        assert_ne!(compute_template_hash(&cargo), compute_template_hash(&pnpm));
        assert_ne!(compute_template_hash(&cargo), compute_template_hash(&both));
    }

    #[test]
    fn test_template_hash_empty() {
        let empty: Vec<String> = vec![];
        let h = compute_template_hash(&empty);
        // Empty should still produce a valid hash
        assert_eq!(h.len(), 16);
    }

    #[test]
    fn test_parse_version_toml() {
        assert_eq!(
            parse_version_toml("version = \"1.2.3\""),
            Some("1.2.3".to_string())
        );
    }

    #[test]
    fn test_parse_version_toml_whitespace() {
        assert_eq!(
            parse_version_toml("  version  =  \"0.39.12\"  "),
            Some("0.39.12".to_string())
        );
    }

    #[test]
    fn test_parse_version_toml_with_other_fields() {
        let content = "# some comment\nversion = \"2.0.0\"\nother = \"value\"";
        assert_eq!(parse_version_toml(content), Some("2.0.0".to_string()));
    }

    #[test]
    fn test_parse_version_toml_empty() {
        assert_eq!(parse_version_toml(""), None);
    }

    #[test]
    fn test_parse_version_toml_missing_quotes() {
        assert_eq!(parse_version_toml("version = 1.2.3"), None);
    }

    #[test]
    fn test_parse_version_toml_empty_version() {
        assert_eq!(parse_version_toml("version = \"\""), None);
    }

    #[test]
    fn test_parse_version_toml_no_version_key() {
        assert_eq!(parse_version_toml("name = \"foo\""), None);
    }
}

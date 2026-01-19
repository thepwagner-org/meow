//! GitHub mirroring functionality.
//!
//! Copies filtered project content to a local clone of a GitHub repository
//! for review and manual push.

use crate::config;
use crate::markdown::{parse, Frontmatter};
use crate::PROJECTS_DIR;
use anyhow::{bail, Context, Result};
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Hardcoded paths that are always excluded from mirrors.
/// These are git-tracked files that should remain private.
/// Build artifacts (target/, node_modules/, etc.) are already excluded
/// because we use `git ls-files` to enumerate source files.
pub const HARDCODED_EXCLUDES: &[&str] = &["journal/", ".meow.d/", "CLAUDE.md", ".envrc"];

/// MIT license text to inject into mirrored repositories.
const MIT_LICENSE: &str = r#"MIT License

Copyright (c) 2024 Peter Wagner

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
"#;

/// Mirror configuration extracted from README frontmatter.
#[derive(Debug, Clone)]
pub struct MirrorConfig {
    /// GitHub org name
    pub org: String,
    /// GitHub repo name
    pub repo: String,
    /// Additional paths to exclude
    pub exclude: Vec<String>,
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

        Some(Self {
            org: org.to_string(),
            repo: repo.to_string(),
            exclude,
        })
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

/// Mirror status for a single project.
#[derive(Debug)]
pub struct MirrorStatus {
    pub project: String,
    pub config: MirrorConfig,
    pub mirror_exists: bool,
    pub has_changes: bool,
    pub has_unpushed: bool,
}

/// Get status for all mirrored projects.
pub fn get_all_status(root: &Path) -> Result<Vec<MirrorStatus>> {
    let projects = find_mirrored_projects(root)?;
    let mut statuses = Vec::new();

    for (project, config) in projects {
        let path = mirror_path(&config.org, &config.repo)?;
        let exists = path.exists();

        statuses.push(MirrorStatus {
            project,
            config: config.clone(),
            mirror_exists: exists,
            has_changes: exists && has_uncommitted_changes(&path),
            has_unpushed: exists && has_unpushed_commits(&path),
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
}

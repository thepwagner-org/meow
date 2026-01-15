use anyhow::{bail, Context, Result};
use std::collections::HashSet;
use std::fs;
use std::path::Path;
use std::process::Command;
use tracing::info;

use crate::PROJECTS_DIR;

/// Directories that must always be included in sparse checkout
const REQUIRED_DIRS: &[&str] = &["nix"];

/// Check if a sparse-checkout line should be processed (not empty, comment, or negation)
fn is_valid_sparse_line(line: &str) -> bool {
    !line.is_empty() && !line.starts_with('#') && !line.starts_with('!')
}

/// Extract project name from a sparse-checkout line like "projects/foo" or "/projects/foo/"
fn extract_project_name(line: &str) -> Option<&str> {
    let prefix = format!("{}/", PROJECTS_DIR);
    let trimmed = line.trim_matches('/');
    trimmed.strip_prefix(&prefix)
}

fn parse_focused_projects(content: &str) -> Vec<String> {
    let mut projects: Vec<_> = content
        .lines()
        .filter(|line| is_valid_sparse_line(line))
        .filter_map(|line| extract_project_name(line).map(|s| s.to_string()))
        .collect::<HashSet<_>>()
        .into_iter()
        .collect();
    projects.sort();
    projects
}

fn ensure_sparse_checkout(repo_root: &Path) -> Result<()> {
    let sparse_file = repo_root.join(".git/info/sparse-checkout");
    if !sparse_file.exists() {
        let output = Command::new("git")
            .current_dir(repo_root)
            .args(["sparse-checkout", "init", "--cone"])
            .output()
            .context("Failed to run git sparse-checkout init")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("git sparse-checkout init failed: {}", stderr);
        }

        // Add required directories
        let output = Command::new("git")
            .current_dir(repo_root)
            .args(["sparse-checkout", "add"])
            .args(REQUIRED_DIRS)
            .output()
            .context("Failed to add required directories to sparse-checkout")?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("Failed to add required directories: {}", stderr);
        }

        info!("Initialized sparse-checkout");
    }
    Ok(())
}

pub fn get_focused_projects(repo_root: &Path) -> Result<Vec<String>> {
    ensure_sparse_checkout(repo_root)?;

    let sparse_file = repo_root.join(".git/info/sparse-checkout");
    let content =
        fs::read_to_string(&sparse_file).context("Failed to read sparse-checkout file")?;
    Ok(parse_focused_projects(&content))
}

pub fn add_project(repo_root: &Path, project: &str) -> Result<()> {
    ensure_sparse_checkout(repo_root)?;

    // Skip if already focused
    let focused = get_focused_projects(repo_root)?;
    if focused.contains(&project.to_string()) {
        return Ok(());
    }

    let project_path = format!("{}/{}", PROJECTS_DIR, project);
    let output = Command::new("git")
        .current_dir(repo_root)
        .args(["sparse-checkout", "add", &project_path])
        .output()
        .context("Failed to run git sparse-checkout add")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git sparse-checkout add failed: {}", stderr);
    }

    Ok(())
}

pub fn drop_project(repo_root: &Path, project: &str) -> Result<()> {
    ensure_sparse_checkout(repo_root)?;

    let sparse_file = repo_root.join(".git/info/sparse-checkout");
    let content =
        fs::read_to_string(&sparse_file).context("Failed to read sparse-checkout file")?;

    let mut dirs: Vec<String> = content
        .lines()
        .filter(|line| is_valid_sparse_line(line) && !line.contains('*'))
        .filter_map(|line| {
            let trimmed = line.trim_matches('/');
            // Skip bare "projects" dir
            if trimmed == PROJECTS_DIR {
                return None;
            }
            // Skip the project being dropped
            if let Some(proj_name) = extract_project_name(line) {
                if proj_name == project {
                    return None;
                }
            }
            Some(trimmed.to_string())
        })
        .collect();

    // Ensure required directories are always included
    for required in REQUIRED_DIRS {
        let req_str = (*required).to_string();
        if !dirs.contains(&req_str) {
            dirs.push(req_str);
        }
    }

    let output = Command::new("git")
        .current_dir(repo_root)
        .args(["sparse-checkout", "set"])
        .args(&dirs)
        .output()
        .context("Failed to run git sparse-checkout set")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git sparse-checkout set failed: {}", stderr);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty() {
        assert_eq!(parse_focused_projects(""), Vec::<String>::new());
    }

    #[test]
    fn test_parse_with_comments_and_empty_lines() {
        let content = "# comment\n\n!negation\n";
        assert_eq!(parse_focused_projects(content), Vec::<String>::new());
    }

    #[test]
    fn test_parse_projects() {
        let content = "projects/foo\nprojects/bar\n";
        assert_eq!(parse_focused_projects(content), vec!["bar", "foo"]);
    }

    #[test]
    fn test_parse_projects_with_slashes() {
        let content = "/projects/foo/\n/projects/bar\n";
        assert_eq!(parse_focused_projects(content), vec!["bar", "foo"]);
    }

    #[test]
    fn test_parse_ignores_non_projects() {
        let content = "src\nlib\nprojects/meow\ndocs\n";
        assert_eq!(parse_focused_projects(content), vec!["meow"]);
    }

    #[test]
    fn test_parse_mixed_content() {
        let content =
            "# sparse checkout config\n\nprojects/alpha\n!exclude\nprojects/beta\nother/path\n";
        assert_eq!(parse_focused_projects(content), vec!["alpha", "beta"]);
    }

    #[test]
    fn test_parse_deduplicates() {
        let content = "projects/foo\nprojects/bar\nprojects/foo\nprojects/bar\nprojects/foo\n";
        assert_eq!(parse_focused_projects(content), vec!["bar", "foo"]);
    }
}

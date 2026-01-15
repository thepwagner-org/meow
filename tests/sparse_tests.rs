use std::fs;
use std::process::Command;
use tempfile::TempDir;

fn create_test_repo_with_sparse() -> TempDir {
    let dir = TempDir::new().unwrap();

    // Initialize git repo
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["init"])
        .output()
        .unwrap();

    // Configure user for commits
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["config", "user.email", "test@example.com"])
        .output()
        .unwrap();
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["config", "user.name", "Test"])
        .output()
        .unwrap();

    // Create some project directories
    fs::create_dir_all(dir.path().join("projects/alpha")).unwrap();
    fs::create_dir_all(dir.path().join("projects/beta")).unwrap();
    fs::write(dir.path().join("projects/alpha/README.md"), "# Alpha").unwrap();
    fs::write(dir.path().join("projects/beta/README.md"), "# Beta").unwrap();

    // Initial commit
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["add", "."])
        .output()
        .unwrap();
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["commit", "-m", "Initial"])
        .output()
        .unwrap();

    // Initialize sparse checkout
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["sparse-checkout", "init", "--cone"])
        .output()
        .unwrap();

    dir
}

#[test]
fn test_get_focused_projects_auto_init() {
    let dir = TempDir::new().unwrap();

    // Initialize git repo without sparse checkout
    let _ = Command::new("git")
        .current_dir(dir.path())
        .args(["init"])
        .output()
        .unwrap();

    // Should auto-initialize sparse checkout and return empty list
    let projects = meow::sparse::get_focused_projects(dir.path()).unwrap();
    assert!(projects.is_empty());

    // Verify sparse-checkout file was created
    assert!(dir.path().join(".git/info/sparse-checkout").exists());
}

#[test]
fn test_get_focused_projects_empty() {
    let dir = create_test_repo_with_sparse();

    // Sparse checkout file exists but has no projects
    let projects = meow::sparse::get_focused_projects(dir.path()).unwrap();
    // By default sparse-checkout init --cone gives us an empty or root-only config
    // which should return no projects under projects/
    assert!(projects.is_empty() || !projects.iter().any(|p| p.contains("projects/")));
}

#[test]
fn test_add_and_get_project() {
    let dir = create_test_repo_with_sparse();

    meow::sparse::add_project(dir.path(), "alpha").unwrap();

    let projects = meow::sparse::get_focused_projects(dir.path()).unwrap();
    assert!(projects.contains(&"alpha".to_string()));
}

#[test]
fn test_add_multiple_projects() {
    let dir = create_test_repo_with_sparse();

    meow::sparse::add_project(dir.path(), "alpha").unwrap();
    meow::sparse::add_project(dir.path(), "beta").unwrap();

    let projects = meow::sparse::get_focused_projects(dir.path()).unwrap();
    assert!(projects.contains(&"alpha".to_string()));
    assert!(projects.contains(&"beta".to_string()));
}

#[test]
fn test_drop_project() {
    let dir = create_test_repo_with_sparse();

    meow::sparse::add_project(dir.path(), "alpha").unwrap();
    meow::sparse::add_project(dir.path(), "beta").unwrap();

    meow::sparse::drop_project(dir.path(), "alpha").unwrap();

    let projects = meow::sparse::get_focused_projects(dir.path()).unwrap();
    assert!(!projects.contains(&"alpha".to_string()));
    assert!(projects.contains(&"beta".to_string()));
}

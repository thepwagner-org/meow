use git2::{Repository, Signature};
use std::fs;
use tempfile::TempDir;

fn create_test_repo() -> (TempDir, Repository) {
    let dir = TempDir::new().unwrap();
    let repo = Repository::init(dir.path()).unwrap();

    // Create initial commit so HEAD exists
    {
        let sig = Signature::now("Test", "test@example.com").unwrap();
        let tree_id = repo.index().unwrap().write_tree().unwrap();
        let tree = repo.find_tree(tree_id).unwrap();
        let _ = repo
            .commit(Some("HEAD"), &sig, &sig, "Initial commit", &tree, &[])
            .unwrap();
    }

    (dir, repo)
}

fn add_project(dir: &TempDir, repo: &Repository, name: &str) {
    let project_path = dir.path().join("projects").join(name);
    fs::create_dir_all(&project_path).unwrap();
    fs::write(project_path.join("README.md"), "# Test").unwrap();

    let mut index = repo.index().unwrap();
    index
        .add_path(std::path::Path::new(&format!(
            "projects/{}/README.md",
            name
        )))
        .unwrap();
    index.write().unwrap();

    let sig = Signature::now("Test", "test@example.com").unwrap();
    let tree_id = index.write_tree().unwrap();
    let tree = repo.find_tree(tree_id).unwrap();
    let parent = repo.head().unwrap().peel_to_commit().unwrap();
    let _ = repo
        .commit(
            Some("HEAD"),
            &sig,
            &sig,
            &format!("Add project {}", name),
            &tree,
            &[&parent],
        )
        .unwrap();
}

#[test]
fn test_list_all_projects_empty() {
    let (_dir, repo) = create_test_repo();
    let projects = meow::git::list_all_projects(&repo).unwrap();
    assert!(projects.is_empty());
}

#[test]
fn test_list_all_projects_single() {
    let (dir, repo) = create_test_repo();
    add_project(&dir, &repo, "alpha");

    let projects = meow::git::list_all_projects(&repo).unwrap();
    assert_eq!(projects, vec!["alpha"]);
}

#[test]
fn test_list_all_projects_multiple_sorted() {
    let (dir, repo) = create_test_repo();
    add_project(&dir, &repo, "zebra");
    add_project(&dir, &repo, "alpha");
    add_project(&dir, &repo, "beta");

    let projects = meow::git::list_all_projects(&repo).unwrap();
    assert_eq!(projects, vec!["alpha", "beta", "zebra"]);
}

#[test]
fn test_repo_root() {
    let (dir, repo) = create_test_repo();
    let root = meow::git::repo_root(&repo).unwrap();
    // Canonicalize both paths to handle symlinks (e.g., /var vs /private/var on macOS)
    let expected = dir.path().canonicalize().unwrap();
    let actual = root.canonicalize().unwrap();
    assert_eq!(actual, expected);
}

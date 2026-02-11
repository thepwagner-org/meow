//! README file parsing, validation, and formatting.
//!
//! README files live at `projects/{name}/README.md` with required YAML frontmatter.
//! Validation is handled by the unified schema-driven validator via [`super::schema::builtin_readme`].

pub use super::Frontmatter;
use super::{custom, parse, schema, Document, FormatContext, ValidationError};
use crate::{git, PROJECTS_DIR};
use anyhow::{Context, Result};
use git2::Repository;
use std::fs;
use std::path::Path;

/// Validate a README document.
///
/// Returns both unfixable errors and fixable issues (which will be auto-fixed).
pub fn validate(doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let type_def = schema::builtin_readme();
    custom::validate_builtin(doc, ctx, &type_def)
}

/// Parse a README file from a path.
pub fn parse_readme(path: &Path) -> Result<Document> {
    let text = fs::read_to_string(path).context("Failed to read README.md")?;
    Ok(parse(&text))
}

/// Load README from filesystem if available, otherwise from git tree.
pub fn load_readme(repo: &Repository, root: &Path, project: &str) -> Result<Document> {
    let fs_path = root.join(PROJECTS_DIR).join(project).join("README.md");
    if fs_path.exists() {
        return parse_readme(&fs_path);
    }
    // Fall back to git tree
    let git_path = format!("{}/{}/README.md", PROJECTS_DIR, project);
    let content = git::read_blob(repo, &git_path)?;
    Ok(parse(&content))
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveDate;

    fn make_ctx() -> FormatContext<'static> {
        FormatContext {
            project: "test",
            path: Path::new("README.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        }
    }

    /// Apply all fixable errors to a document.
    fn apply_fixes(doc: &mut Document, errors: &[ValidationError]) {
        for error in errors {
            error.fix(doc);
        }
    }

    #[test]
    fn test_parse_frontmatter() {
        let text = r#"---
created: 2024-03-15
description: A test project
---
# Test Project

Some content here.
"#;
        let doc = parse(text);
        assert!(doc.frontmatter.is_some());
        let fm = doc.frontmatter.as_ref().expect("frontmatter");
        assert_eq!(
            fm.created,
            Some(NaiveDate::from_ymd_opt(2024, 3, 15).expect("valid date"))
        );
        assert_eq!(fm.description.as_deref(), Some("A test project"));
    }

    #[test]
    fn test_validate_missing_frontmatter() {
        let doc = parse("# Just a heading\n\nSome content.");
        let errors = validate(&doc, &make_ctx());
        assert!(errors
            .iter()
            .any(|e| matches!(e, ValidationError::MissingFrontmatter { .. })));
    }

    #[test]
    fn test_validate_missing_required_fields() {
        let text = r#"---
created: 2024-03-15
---
# Test
"#;
        let doc = parse(text);
        let errors = validate(&doc, &make_ctx());
        assert!(errors.iter().any(|e| e.message().contains("description")));
    }

    #[test]
    fn test_fix_uses_project_name() {
        let text = r#"---
created: 2024-03-15
description: A test project
---
# Wrong Title

Some content.
"#;
        let mut doc = parse(text);
        let ctx = FormatContext {
            project: "my-project",
            path: Path::new("README.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };
        let errors = validate(&doc, &ctx);
        apply_fixes(&mut doc, &errors);
        let formatted = super::super::serialize(&doc);
        assert!(formatted.contains("# my-project\n"));
    }

    #[test]
    fn test_fix_uses_frontmatter_name() {
        let text = r#"---
created: 2024-03-15
description: A test project
name: Custom Name
---
# Wrong Title

Some content.
"#;
        let mut doc = parse(text);
        let errors = validate(&doc, &make_ctx());
        apply_fixes(&mut doc, &errors);
        let formatted = super::super::serialize(&doc);
        assert!(formatted.contains("# Custom Name\n"));
        assert!(!formatted.contains("my-project"));
    }

    #[test]
    fn test_format_idempotent() {
        let text = r#"---
created: 2024-03-15
description: A test project
---

# test

Some content here.
"#;
        let mut doc = parse(text);
        let errors = validate(&doc, &make_ctx());
        apply_fixes(&mut doc, &errors);
        let formatted1 = super::super::serialize(&doc);

        let mut doc2 = parse(&formatted1);
        let errors2 = validate(&doc2, &make_ctx());
        apply_fixes(&mut doc2, &errors2);
        let formatted2 = super::super::serialize(&doc2);

        assert_eq!(formatted1, formatted2);
    }
}

//! Agent command file validation and formatting.
//!
//! Command files (`.claude/commands/*.md`, `.opencode/command/*.md`) define custom
//! slash commands. Each command file should have an H1 title.
//!
//! Validation is handled by the unified schema-driven validator via [`super::schema::builtin_command`].

use super::{custom, schema, Document, FormatContext, ValidationError};

/// Validate a command document.
///
/// Returns both unfixable errors and fixable issues (which will be auto-fixed).
pub fn validate(doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let type_def = schema::builtin_command();
    custom::validate_builtin(doc, ctx, &type_def)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::{parse, FileType};
    use std::path::Path;
    use tempfile::NamedTempFile;

    #[test]
    fn test_matches_claude_command() {
        assert!(matches!(
            FileType::detect(Path::new(".claude/commands/shipit.md"), None),
            Some(FileType::AgentCommand)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/.claude/commands/test.md"), None),
            Some(FileType::AgentCommand)
        ));
        // Not a command file
        assert!(!matches!(
            FileType::detect(Path::new(".claude/settings.json"), None),
            Some(FileType::AgentCommand)
        ));
        // Commands must be in .claude/commands/
        assert!(!matches!(
            FileType::detect(Path::new(".claude/shipit.md"), None),
            Some(FileType::AgentCommand)
        ));
    }

    #[test]
    fn test_matches_opencode_command() {
        // .opencode/command/ (singular)
        assert!(matches!(
            FileType::detect(Path::new(".opencode/command/shipit.md"), None),
            Some(FileType::AgentCommand)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/.opencode/command/test.md"), None),
            Some(FileType::AgentCommand)
        ));
        // Not a command file
        assert!(!matches!(
            FileType::detect(Path::new(".opencode/settings.json"), None),
            Some(FileType::AgentCommand)
        ));
        // Commands must be in .opencode/command/ (not commands plural)
        assert!(!matches!(
            FileType::detect(Path::new(".opencode/commands/shipit.md"), None),
            Some(FileType::AgentCommand)
        ));
        assert!(!matches!(
            FileType::detect(Path::new(".opencode/shipit.md"), None),
            Some(FileType::AgentCommand)
        ));
    }

    #[test]
    fn test_validate_requires_h1() {
        let doc = parse("Just some content without a heading.");
        let file = NamedTempFile::new().expect("tempfile");
        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(errors.iter().any(|e| e.message().contains("H1 title")));
    }

    #[test]
    fn test_validate_h1_ok() {
        let doc = parse("# Ship It\n\nRun tests, build, and commit.");
        let file = NamedTempFile::new().expect("tempfile");
        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(errors.is_empty());
    }
}

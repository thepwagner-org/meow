//! Claude command file validation and formatting.
//!
//! Claude command files (`.claude/commands/*.md`) define custom slash commands.
//! Each command file should have an H1 title describing what the command does.

use super::{Block, Document, FormatContext, ValidationError};

/// Validate a Claude command document.
pub fn validate(doc: &Document, _ctx: &FormatContext) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    // Check for H1 title
    let has_h1 = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::Heading { level: 1, .. }));
    if !has_h1 {
        errors.push(ValidationError {
            line: 0,
            message: "Claude command should have an H1 title describing the command".to_string(),
        });
    }

    errors
}

/// Normalize a Claude command document in place.
pub fn normalize(_doc: &mut Document, _ctx: &FormatContext) {
    // No normalization needed - just re-serialize for consistent formatting
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
            Some(FileType::ClaudeCommand)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/.claude/commands/test.md"), None),
            Some(FileType::ClaudeCommand)
        ));
        // Not a command file
        assert!(!matches!(
            FileType::detect(Path::new(".claude/settings.json"), None),
            Some(FileType::ClaudeCommand)
        ));
        // Commands must be in .claude/commands/
        assert!(!matches!(
            FileType::detect(Path::new(".claude/shipit.md"), None),
            Some(FileType::ClaudeCommand)
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
        assert!(errors.iter().any(|e| e.message.contains("H1 title")));
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

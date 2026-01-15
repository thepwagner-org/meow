//! ROADMAP.md file validation and formatting.
//!
//! ROADMAP.md files document future possibilities and non-goals.
//! Validates structure and ensures Non-Goals section exists.

use super::{Block, Document, FormatContext, Inline, ValidationError};

/// Expected non-goals intro text.
const NON_GOALS_INTRO: &str = "Explicitly out of scope";

/// Validate a ROADMAP.md document.
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
            message: "ROADMAP.md should have an H1 title".to_string(),
        });
    }

    // Check for Non-Goals section
    let has_non_goals = doc.blocks.iter().any(|b| {
        matches!(b, Block::Heading { level: 2, .. })
            && b.heading_text()
                .map(|t| t.eq_ignore_ascii_case("non-goals"))
                .unwrap_or(false)
    });
    if !has_non_goals {
        errors.push(ValidationError {
            line: 0,
            message: "ROADMAP.md should have a '## Non-Goals' section".to_string(),
        });
    }

    errors
}

/// Normalize a ROADMAP.md document in place.
pub fn normalize(doc: &mut Document, _ctx: &FormatContext) {
    // Find Non-Goals section and ensure it has the standard intro
    if let Some(idx) = doc.blocks.iter().position(|b| {
        matches!(b, Block::Heading { level: 2, .. })
            && b.heading_text()
                .map(|t| t.eq_ignore_ascii_case("non-goals"))
                .unwrap_or(false)
    }) {
        // Find the first non-blank block after the heading
        let content_idx = doc.blocks[idx + 1..]
            .iter()
            .position(|b| !matches!(b, Block::BlankLine))
            .map(|i| idx + 1 + i);

        // Check if it's a paragraph starting with the intro
        let has_intro = content_idx.is_some_and(|i| {
            if let Some(Block::Paragraph(inlines)) = doc.blocks.get(i) {
                let text = super::inlines_to_string(inlines);
                text.starts_with(NON_GOALS_INTRO)
            } else {
                false
            }
        });

        // If no intro paragraph, insert one after heading (and any blank line)
        if !has_intro {
            let insert_idx = if matches!(doc.blocks.get(idx + 1), Some(Block::BlankLine)) {
                idx + 2
            } else {
                idx + 1
            };

            let intro = Block::Paragraph(vec![Inline::Text(format!(
                "{} to keep the project focused:",
                NON_GOALS_INTRO
            ))]);

            doc.blocks.insert(insert_idx, intro);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::{parse, serialize, FileType};
    use std::path::Path;
    use tempfile::NamedTempFile;

    #[test]
    fn test_matches_roadmap_md() {
        assert!(matches!(
            FileType::detect(Path::new("ROADMAP.md"), None),
            Some(FileType::Roadmap)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/ROADMAP.md"), None),
            Some(FileType::Roadmap)
        ));
        assert!(!matches!(
            FileType::detect(Path::new("README.md"), None),
            Some(FileType::Roadmap)
        ));
        assert!(FileType::detect(Path::new("roadmap.md"), None).is_none()); // case sensitive
    }

    #[test]
    fn test_validate_missing_h1() {
        let doc = parse("## Section\n\nSome content.");
        let file = NamedTempFile::new().expect("tempfile");
        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(errors.iter().any(|e| e.message.contains("H1")));
    }

    #[test]
    fn test_validate_missing_non_goals() {
        let doc = parse("# Roadmap\n\n## Features\n\nSome features.");
        let file = NamedTempFile::new().expect("tempfile");
        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(errors.iter().any(|e| e.message.contains("Non-Goals")));
    }

    #[test]
    fn test_validate_ok() {
        let content = "# Roadmap\n\n## Features\n\n## Non-Goals\n\nOut of scope.";
        let doc = parse(content);
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

    fn normalize_test(content: &str) -> String {
        let mut doc = parse(content);
        let file = NamedTempFile::new().expect("tempfile");
        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };
        normalize(&mut doc, &ctx);
        serialize(&doc)
    }

    #[test]
    fn test_normalize_adds_intro_to_non_goals() {
        let content = "# Roadmap\n\n## Non-Goals\n\n- **Thing** - Reason";
        let result = normalize_test(content);
        assert!(result.contains("Explicitly out of scope"));
    }

    #[test]
    fn test_normalize_preserves_existing_intro() {
        let content =
            "# Roadmap\n\n## Non-Goals\n\nExplicitly out of scope to keep the project focused:\n\n- **Thing** - Reason";
        let result = normalize_test(content);
        // Should only have one instance
        assert_eq!(result.matches("Explicitly out of scope").count(), 1);
    }

    #[test]
    fn test_normalize_idempotent() {
        let content = "# Roadmap\n\n## Non-Goals\n\n- **Thing** - Reason";
        let first = normalize_test(content);
        let second = normalize_test(&first);
        assert_eq!(first, second);
    }
}

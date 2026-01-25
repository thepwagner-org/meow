//! CLAUDE.md file validation and formatting.
//!
//! CLAUDE.md files provide instructions to Claude Code. No frontmatter required,
//! but warns if the file is too large (soft limit).

use super::{parse, Block, Document, FormatContext, ValidationError};

/// Soft limit for CLAUDE.md file size (in bytes).
const SIZE_WARNING_THRESHOLD: usize = 4000;

/// Canonical related documents section injected into CLAUDE.md files.
const RELATED_DOCS_SECTION: &str = r#"## Related Documents

- **journal/** - Daily notes in `YYYY-MM.md` files. Log design discussions, investigations, and decisions with `## YYYY-MM-DD` headings.
- **ROADMAP.md** - Future possibilities and explicit non-goals. Update when brainstorming; don't track completed work here.
- **CLAUDE.md** - How to work on the project: architecture, commands, lints. Keep concise.
- **../knowledge/** - Personal knowledge base. Read `CLAUDE.md` there for structure; schemas in `.meow.d/`.
"#;

/// Validate a CLAUDE.md document.
pub fn validate(_doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    // Warn if file is large
    if let Ok(content) = std::fs::read_to_string(ctx.path) {
        if content.len() > SIZE_WARNING_THRESHOLD {
            errors.push(ValidationError {
                line: 0,
                message: format!(
                    "CLAUDE.md is large ({} bytes, threshold: {}). Consider condensing.",
                    content.len(),
                    SIZE_WARNING_THRESHOLD
                ),
            });
        }
    }

    errors
}

/// Normalize a CLAUDE.md document in place.
pub fn normalize(doc: &mut Document, _ctx: &FormatContext) {
    let template_doc = parse(RELATED_DOCS_SECTION);
    let template_blocks: Vec<Block> = template_doc.blocks;
    // Count non-blank blocks for comparison with existing content
    let template_content_count = template_blocks
        .iter()
        .filter(|b| !matches!(b, Block::BlankLine))
        .count();

    // Find existing Related Documents section (or legacy Journal/Roadmap sections)
    let section_idx = find_related_docs_section(doc);

    match section_idx {
        Some((idx, legacy_sections)) => {
            // Find the end of all related sections (next H2 after last legacy section, or end of doc)
            let last_section_idx = legacy_sections.last().copied().unwrap_or(idx);
            let section_end = doc.blocks[last_section_idx + 1..]
                .iter()
                .position(|b| matches!(b, Block::Heading { level: 2, .. }))
                .map(|i| last_section_idx + 1 + i)
                .unwrap_or(doc.blocks.len());

            // Extract existing content blocks (non-blank) in the section
            let existing_content: Vec<Block> = doc.blocks[idx..section_end]
                .iter()
                .filter(|b| !matches!(b, Block::BlankLine))
                .cloned()
                .collect();

            // Preserve any additional blocks beyond the template (custom notes)
            let custom_content: Vec<Block> = if existing_content.len() > template_content_count {
                existing_content[template_content_count..].to_vec()
            } else {
                vec![]
            };

            // Build new section: template + blank + custom content
            let mut new_section = template_blocks;
            if !custom_content.is_empty() {
                new_section.push(Block::BlankLine);
                new_section.extend(custom_content);
            }

            // Replace the section(s)
            let _ = doc.blocks.splice(idx..section_end, new_section);
        }
        None => {
            // Append related docs section at the end
            if !doc.blocks.is_empty() && !matches!(doc.blocks.last(), Some(Block::BlankLine)) {
                doc.blocks.push(Block::BlankLine);
            }
            doc.blocks.extend(template_blocks);
        }
    }
}

/// Find the Related Documents section, or legacy Journal/Roadmap sections to migrate.
/// Returns the index of the first section found and a list of all legacy section indices.
fn find_related_docs_section(doc: &Document) -> Option<(usize, Vec<usize>)> {
    let mut legacy_sections = Vec::new();
    let mut first_idx = None;

    for (i, block) in doc.blocks.iter().enumerate() {
        if let Block::Heading { level: 2, .. } = block {
            let text = block.heading_text().unwrap_or_default();
            let text_lower = text.to_lowercase();

            if text_lower == "related documents" {
                // Found the canonical section
                return Some((i, vec![]));
            }

            if text_lower == "journal" || text_lower == "roadmap" {
                if first_idx.is_none() {
                    first_idx = Some(i);
                }
                legacy_sections.push(i);
            }
        }
    }

    first_idx.map(|idx| (idx, legacy_sections))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::{parse, serialize, FileType};
    use std::io::Write;
    use std::path::Path;
    use tempfile::NamedTempFile;

    #[test]
    fn test_matches_claude_md() {
        assert!(matches!(
            FileType::detect(Path::new("CLAUDE.md"), None),
            Some(FileType::Claude)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/CLAUDE.md"), None),
            Some(FileType::Claude)
        ));
        assert!(!matches!(
            FileType::detect(Path::new("README.md"), None),
            Some(FileType::Claude)
        ));
        assert!(FileType::detect(Path::new("claude.md"), None).is_none()); // case sensitive
    }

    #[test]
    fn test_validate_no_heading_ok() {
        let doc = parse("Just some content without a heading.");

        let mut file = NamedTempFile::new().expect("tempfile");
        write!(file, "Just some content").expect("write");

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

    #[test]
    fn test_validate_large_file() {
        let content = "# Test\n".to_string() + &"x".repeat(5000);
        let doc = parse(&content);

        let mut file = NamedTempFile::new().expect("tempfile");
        write!(file, "{}", content).expect("write");

        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(errors.iter().any(|e| e.message.contains("large")));
    }

    #[test]
    fn test_validate_ok() {
        let content = "# meow\n\nSome instructions.";
        let doc = parse(content);

        let mut file = NamedTempFile::new().expect("tempfile");
        write!(file, "{}", content).expect("write");

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
    fn test_normalize_adds_related_docs_to_empty() {
        let result = normalize_test("");
        assert!(result.contains("## Related Documents"));
        assert!(result.contains("journal/"));
        assert!(result.contains("ROADMAP.md"));
        assert!(result.contains("CLAUDE.md"));
    }

    #[test]
    fn test_normalize_adds_related_docs_to_existing_content() {
        let result = normalize_test("# meow\n\nSome instructions.");
        assert!(result.contains("# meow"));
        assert!(result.contains("Some instructions"));
        assert!(result.contains("## Related Documents"));
        // Related Documents should be at the end
        let related_pos = result
            .find("## Related Documents")
            .expect("related docs section");
        let instructions_pos = result.find("Some instructions").expect("instructions");
        assert!(related_pos > instructions_pos);
    }

    #[test]
    fn test_normalize_migrates_legacy_journal() {
        let content = "# meow\n\n## Journal\n\nOld content here.";
        let result = normalize_test(content);
        // Should have the new section
        assert!(result.contains("## Related Documents"));
        // Should not have the old Journal heading
        assert!(!result.contains("## Journal"));
        // Should not have the old content
        assert!(!result.contains("Old content here"));
    }

    #[test]
    fn test_normalize_migrates_legacy_roadmap() {
        let content = "# meow\n\n## Roadmap\n\nSome roadmap content.";
        let result = normalize_test(content);
        // Should have the new section
        assert!(result.contains("## Related Documents"));
        // Should not have the old Roadmap heading
        assert!(!result.contains("## Roadmap"));
    }

    #[test]
    fn test_normalize_preserves_custom_content() {
        // Create content with Related Documents section + custom content beyond template
        let content = r#"# meow

## Related Documents

- **journal/** - Daily notes in `YYYY-MM.md` files. Log design discussions, investigations, and decisions with `## YYYY-MM-DD` headings.
- **ROADMAP.md** - Future possibilities and explicit non-goals. Update when brainstorming; don't track completed work here.
- **CLAUDE.md** - How to work on the project: architecture, commands, lints. Keep concise.

### Project-specific notes

This project also has X, Y, Z.
"#;
        let result = normalize_test(content);
        // Should preserve custom content
        assert!(result.contains("Project-specific notes"));
        assert!(result.contains("This project also has"));
    }

    #[test]
    fn test_normalize_idempotent() {
        let content = "# meow\n\nSome instructions.";
        let first = normalize_test(content);
        let second = normalize_test(&first);
        assert_eq!(first, second);
    }
}

//! Agent instruction file validation and formatting (AGENTS.md, CLAUDE.md).
//!
//! These files provide instructions to AI coding agents. No frontmatter required,
//! but warns if the file is too large (soft limit).
//!
//! AGENTS.md is the canonical format (for opencode and other agents).
//! CLAUDE.md is supported as a fallback for Claude Code.

use super::{parse, Block, Document, FormatContext, ValidationError};

/// Soft limit for agent instruction file size (in bytes).
const SIZE_WARNING_THRESHOLD: usize = 4000;

/// Template for related documents section. `{AGENT_FILE}` is replaced with the actual filename.
const RELATED_DOCS_TEMPLATE: &str = r#"## Related Documents

- **journal/** - Daily notes in `YYYY-MM.md` files. Log design discussions, investigations, and decisions with `## YYYY-MM-DD` headings.
- **ROADMAP.md** - Future possibilities and explicit non-goals. Update when brainstorming; don't track completed work here.
- **{AGENT_FILE}** - How to work on the project: architecture, commands, lints. Keep concise.
- **../knowledge/** - Personal knowledge base. Read `CLAUDE.md` there for structure; schemas in `.meow.d/`.
"#;

/// Validate an agent instruction file (AGENTS.md or CLAUDE.md).
///
/// The `agent_file` parameter specifies which filename to use in the Related Documents
/// template (e.g., "AGENTS.md" or "CLAUDE.md").
///
/// Returns both unfixable errors and fixable issues (which will be auto-fixed).
pub fn validate(doc: &Document, ctx: &FormatContext, agent_file: &str) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    // Warn if file is large
    if let Ok(content) = std::fs::read_to_string(ctx.path) {
        if content.len() > SIZE_WARNING_THRESHOLD {
            errors.push(ValidationError::FileTooLarge {
                line: 1,
                size: content.len(),
                threshold: SIZE_WARNING_THRESHOLD,
            });
        }
    }

    // Check if Related Documents section needs updating
    let related_docs_section = RELATED_DOCS_TEMPLATE.replace("{AGENT_FILE}", agent_file);
    let template_doc = parse(&related_docs_section);
    let template_blocks: Vec<Block> = template_doc.blocks;
    let template_content_count = template_blocks
        .iter()
        .filter(|b| !matches!(b, Block::BlankLine))
        .count();

    let section_idx = find_related_docs_section(doc);

    match section_idx {
        Some((idx, legacy_sections)) => {
            // Find the end of all related sections
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

            // Check if content needs updating (legacy sections or template mismatch)
            let needs_update = !legacy_sections.is_empty() || {
                // Compare existing template portion with expected
                let existing_template_portion: Vec<_> = existing_content
                    .iter()
                    .take(template_content_count)
                    .collect();
                let expected_template_portion: Vec<_> = template_blocks
                    .iter()
                    .filter(|b| !matches!(b, Block::BlankLine))
                    .collect();
                existing_template_portion.len() != expected_template_portion.len()
            };

            if needs_update {
                errors.push(ValidationError::RelatedDocsNeedsUpdate {
                    section_start: Some(idx),
                    section_end,
                    template_blocks,
                    custom_content,
                });
            }
        }
        None => {
            // Section doesn't exist at all
            errors.push(ValidationError::RelatedDocsNeedsUpdate {
                section_start: None,
                section_end: doc.blocks.len(),
                template_blocks,
                custom_content: vec![],
            });
        }
    }

    errors
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

    /// Apply all fixable errors to a document.
    fn apply_fixes(doc: &mut Document, errors: &[ValidationError]) {
        for error in errors {
            error.fix(doc);
        }
    }

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
    fn test_matches_agents_md() {
        assert!(matches!(
            FileType::detect(Path::new("AGENTS.md"), None),
            Some(FileType::Agents)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/AGENTS.md"), None),
            Some(FileType::Agents)
        ));
        assert!(FileType::detect(Path::new("agents.md"), None).is_none()); // case sensitive
    }

    #[test]
    fn test_agents_md_template_uses_agents() {
        let result = fix_test_with_file("", "AGENTS.md");
        assert!(result.contains("## Related Documents"));
        // Should have AGENTS.md as the agent file entry
        assert!(result.contains("- **AGENTS.md** - How to work on the project"));
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

        let errors = validate(&doc, &ctx, "CLAUDE.md");
        // Should only have fixable issues (Related Documents needs adding)
        assert!(errors.iter().all(|e| e.is_fixable()));
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

        let errors = validate(&doc, &ctx, "CLAUDE.md");
        assert!(errors
            .iter()
            .any(|e| matches!(e, ValidationError::FileTooLarge { .. })));
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

        let errors = validate(&doc, &ctx, "CLAUDE.md");
        // Should only have fixable issues
        assert!(errors.iter().all(|e| e.is_fixable()));
    }

    fn fix_test(content: &str) -> String {
        fix_test_with_file(content, "CLAUDE.md")
    }

    fn fix_test_with_file(content: &str, agent_file: &str) -> String {
        let mut doc = parse(content);
        let file = NamedTempFile::new().expect("tempfile");
        let ctx = FormatContext {
            project: "test",
            path: file.path(),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };
        let errors = validate(&doc, &ctx, agent_file);
        apply_fixes(&mut doc, &errors);
        serialize(&doc)
    }

    #[test]
    fn test_fix_adds_related_docs_to_empty() {
        let result = fix_test("");
        assert!(result.contains("## Related Documents"));
        assert!(result.contains("journal/"));
        assert!(result.contains("ROADMAP.md"));
        assert!(result.contains("CLAUDE.md"));
    }

    #[test]
    fn test_fix_adds_related_docs_to_existing_content() {
        let result = fix_test("# meow\n\nSome instructions.");
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
    fn test_fix_migrates_legacy_journal() {
        let content = "# meow\n\n## Journal\n\nOld content here.";
        let result = fix_test(content);
        // Should have the new section
        assert!(result.contains("## Related Documents"));
        // Should not have the old Journal heading
        assert!(!result.contains("## Journal"));
        // Should not have the old content
        assert!(!result.contains("Old content here"));
    }

    #[test]
    fn test_fix_migrates_legacy_roadmap() {
        let content = "# meow\n\n## Roadmap\n\nSome roadmap content.";
        let result = fix_test(content);
        // Should have the new section
        assert!(result.contains("## Related Documents"));
        // Should not have the old Roadmap heading
        assert!(!result.contains("## Roadmap"));
    }

    #[test]
    fn test_fix_preserves_custom_content() {
        // Create content with Related Documents section + custom content beyond template
        let content = r#"# meow

## Related Documents

- **journal/** - Daily notes in `YYYY-MM.md` files. Log design discussions, investigations, and decisions with `## YYYY-MM-DD` headings.
- **ROADMAP.md** - Future possibilities and explicit non-goals. Update when brainstorming; don't track completed work here.
- **CLAUDE.md** - How to work on the project: architecture, commands, lints. Keep concise.

### Project-specific notes

This project also has X, Y, Z.
"#;
        let result = fix_test(content);
        // Should preserve custom content
        assert!(result.contains("Project-specific notes"));
        assert!(result.contains("This project also has"));
    }

    #[test]
    fn test_fix_idempotent() {
        let content = "# meow\n\nSome instructions.";
        let first = fix_test(content);
        let second = fix_test(&first);
        assert_eq!(first, second);
    }
}

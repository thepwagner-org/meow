//! Agent skill file validation and formatting.
//!
//! Skill files (`.opencode/skills/*/SKILL.md`, `.claude/skills/*/SKILL.md`) define
//! agent capabilities that can be loaded on demand. Each skill file must have
//! `name` and `description` frontmatter fields, and the `name` must match the
//! parent directory name.
//!
//! Validation is handled by the unified schema-driven validator via [`super::schema::builtin_skill`].

use super::{custom, schema, Document, FormatContext, ValidationError};

/// Validate a skill document.
///
/// Returns both unfixable errors and fixable issues (which will be auto-fixed).
pub fn validate(doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let type_def = schema::builtin_skill();
    let mut errors = custom::validate_builtin(doc, ctx, &type_def);

    // Validate that name field matches parent directory name
    if let Some(ref fm) = doc.frontmatter {
        if let Some(ref name) = fm.name {
            if let Some(parent) = ctx.path.parent() {
                if let Some(dir_name) = parent.file_name().and_then(|n| n.to_str()) {
                    if name != dir_name {
                        errors.push(ValidationError::SchemaError {
                            line: 1,
                            message: format!(
                                "skill name '{}' must match directory name '{}'",
                                name, dir_name
                            ),
                        });
                    }
                }
            }
        }
    }

    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::{parse, FileType};
    use std::path::Path;
    use tempfile::TempDir;

    #[test]
    fn test_matches_opencode_skill() {
        assert!(matches!(
            FileType::detect(Path::new(".opencode/skills/my-skill/SKILL.md"), None),
            Some(FileType::AgentSkill)
        ));
        assert!(matches!(
            FileType::detect(
                Path::new("/some/path/.opencode/skills/query/SKILL.md"),
                None
            ),
            Some(FileType::AgentSkill)
        ));
    }

    #[test]
    fn test_matches_claude_skill() {
        assert!(matches!(
            FileType::detect(Path::new(".claude/skills/my-skill/SKILL.md"), None),
            Some(FileType::AgentSkill)
        ));
        assert!(matches!(
            FileType::detect(Path::new("/some/path/.claude/skills/query/SKILL.md"), None),
            Some(FileType::AgentSkill)
        ));
    }

    #[test]
    fn test_rejects_non_skill_paths() {
        // Bare skills/ without .opencode or .claude parent
        assert!(FileType::detect(Path::new("skills/my-skill/SKILL.md"), None).is_none());
        // Wrong filename
        assert!(
            FileType::detect(Path::new(".opencode/skills/my-skill/README.md"), None).is_none()
                || !matches!(
                    FileType::detect(Path::new(".opencode/skills/my-skill/README.md"), None),
                    Some(FileType::AgentSkill)
                )
        );
        // Not in skills/ directory
        assert!(!matches!(
            FileType::detect(Path::new(".opencode/command/SKILL.md"), None),
            Some(FileType::AgentSkill)
        ));
        // SKILL.md at wrong depth
        assert!(!matches!(
            FileType::detect(Path::new(".opencode/skills/SKILL.md"), None),
            Some(FileType::AgentSkill)
        ));
    }

    #[test]
    fn test_validate_requires_name_and_description() {
        let doc = parse("---\nfoo: bar\n---\n## When To Use\n\nSome content.");
        let tmp = TempDir::new().expect("tempdir");
        let skill_dir = tmp.path().join(".opencode/skills/my-skill");
        std::fs::create_dir_all(&skill_dir).expect("mkdir");
        let skill_path = skill_dir.join("SKILL.md");
        std::fs::write(&skill_path, "").expect("write");

        let ctx = FormatContext {
            project: "test",
            path: &skill_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        let messages: Vec<String> = errors.iter().map(|e| e.message()).collect();
        assert!(
            messages.iter().any(|m| m.contains("'name'")),
            "expected name error, got: {:?}",
            messages
        );
        assert!(
            messages.iter().any(|m| m.contains("'description'")),
            "expected description error, got: {:?}",
            messages
        );
    }

    #[test]
    fn test_validate_name_matches_dir() {
        let doc = parse(
            "---\nname: wrong-name\ndescription: A test skill.\n---\n## When To Use\n\nTest.",
        );
        let tmp = TempDir::new().expect("tempdir");
        let skill_dir = tmp.path().join(".opencode/skills/my-skill");
        std::fs::create_dir_all(&skill_dir).expect("mkdir");
        let skill_path = skill_dir.join("SKILL.md");
        std::fs::write(&skill_path, "").expect("write");

        let ctx = FormatContext {
            project: "test",
            path: &skill_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(
            errors
                .iter()
                .any(|e| e.message().contains("must match directory name")),
            "expected name-matches-dir error, got: {:?}",
            errors.iter().map(|e| e.message()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_validate_valid_skill() {
        let doc =
            parse("---\nname: my-skill\ndescription: A test skill.\n---\n## When To Use\n\nTest.");
        let tmp = TempDir::new().expect("tempdir");
        let skill_dir = tmp.path().join(".opencode/skills/my-skill");
        std::fs::create_dir_all(&skill_dir).expect("mkdir");
        let skill_path = skill_dir.join("SKILL.md");
        std::fs::write(&skill_path, "").expect("write");

        let ctx = FormatContext {
            project: "test",
            path: &skill_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(
            errors.is_empty(),
            "expected no errors, got: {:?}",
            errors.iter().map(|e| e.message()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_validate_skill_with_type_none() {
        // Skills in schema-covered dirs may have type: none — should still validate
        let doc = parse(
            "---\ntype: none\nname: my-skill\ndescription: A test skill.\n---\n## When To Use\n\nTest.",
        );
        let tmp = TempDir::new().expect("tempdir");
        let skill_dir = tmp.path().join(".opencode/skills/my-skill");
        std::fs::create_dir_all(&skill_dir).expect("mkdir");
        let skill_path = skill_dir.join("SKILL.md");
        std::fs::write(&skill_path, "").expect("write");

        let ctx = FormatContext {
            project: "test",
            path: &skill_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx);
        assert!(
            errors.is_empty(),
            "expected no errors, got: {:?}",
            errors.iter().map(|e| e.message()).collect::<Vec<_>>()
        );
    }
}

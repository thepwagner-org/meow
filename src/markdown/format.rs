//! Markdown formatting orchestration.
//!
//! Walks project directories, dispatches to handlers based on filename patterns.

use super::{
    is_encrypted, parse, parse_encrypted, schema, schema::Schema, serialize, serialize_encrypted,
    serialize_with_field_order, FileError, FileType, FormatContext, FormatResult, ValidationError,
};
use crate::PROJECTS_DIR;
use anyhow::{Context, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Options for formatting.
#[derive(Debug, Default, Clone, Copy)]
pub struct FormatOptions {
    /// Skip encrypted files entirely.
    pub skip_encrypted: bool,
}

/// Project encryption configuration resolved from README.md.
#[derive(Debug, Default)]
struct ProjectEncryptConfig {
    /// GPG key ID to use for encryption.
    key_id: Option<String>,
    /// File types that should be encrypted.
    encrypt_files: Vec<String>,
}

impl ProjectEncryptConfig {
    /// Load encryption config from project README.md.
    fn load(project_dir: &Path) -> Self {
        let readme_path = project_dir.join("README.md");
        let Ok(content) = fs::read_to_string(&readme_path) else {
            return Self::default();
        };

        let doc = parse(&content);
        let Some(fm) = doc.frontmatter else {
            return Self::default();
        };

        let Some(encrypt) = fm.encrypt else {
            return Self::default();
        };

        Self {
            key_id: encrypt.key_id,
            encrypt_files: encrypt.files,
        }
    }

    /// Check if a file type should be encrypted.
    fn should_encrypt(&self, file_type: &str) -> bool {
        self.encrypt_files.iter().any(|t| t == file_type)
    }

    /// Get the GPG key ID, falling back to environment variable.
    fn get_key_id(&self) -> Option<String> {
        self.key_id
            .clone()
            .or_else(|| std::env::var("MEOW_GPG_KEY").ok())
    }
}

/// Info about a custom type document for index generation.
struct CustomDocInfo {
    /// Path to the .meow.d directory.
    schema_dir: PathBuf,
    /// Type name from frontmatter.
    type_name: String,
}

/// Format a single markdown file using the appropriate handler.
/// Returns (changed, errors, custom_doc_info).
fn format_file(
    path: &Path,
    ctx: &FormatContext,
    project_dir: &Path,
    encrypt_config: &ProjectEncryptConfig,
    opts: &FormatOptions,
) -> Result<(bool, Vec<ValidationError>, Option<CustomDocInfo>)> {
    let content = fs::read_to_string(path).context("Failed to read markdown file")?;
    let file_is_encrypted = is_encrypted(&content);

    // Skip encrypted files if requested
    if opts.skip_encrypted && file_is_encrypted {
        return Ok((false, vec![], None));
    }

    let Some(file_type) = FileType::detect(path, Some(project_dir)) else {
        return Ok((false, vec![], None)); // No handler for this file
    };

    // README.md should only be formatted at the project root
    if matches!(file_type, FileType::Readme) {
        let expected_suffix = format!("{}/{}/README.md", PROJECTS_DIR, ctx.project);
        if !path.ends_with(&expected_suffix) {
            return Ok((false, vec![], None));
        }
    }

    // Determine if this file should be encrypted (from config or schema)
    let should_encrypt = determine_should_encrypt(&file_type, encrypt_config);

    // Parse content (decrypting if needed)
    let (mut doc, original_body_b64) = if file_is_encrypted {
        parse_encrypted(&content)?
    } else {
        (parse(&content), None)
    };

    let errors = file_type.validate(&doc, ctx);
    if !errors.is_empty() {
        return Ok((false, errors, None));
    }

    file_type.normalize(&mut doc, ctx);

    // Track custom doc info for index generation
    let custom_info = match &file_type {
        FileType::Custom { type_name, .. } => {
            Schema::find_for_path(path, project_dir).map(|(_, schema_dir)| CustomDocInfo {
                schema_dir,
                type_name: type_name.clone(),
            })
        }
        _ => None,
    };

    // Get field order for schema types
    let field_order: Option<Vec<&str>> = match &file_type {
        FileType::Custom { schema, type_name } => schema.get_type(type_name).map(|type_def| {
            let mut order: Vec<&str> = vec!["type"];
            // Include structure.frontmatter fields (preserves schema order)
            order.extend(
                type_def
                    .structure
                    .frontmatter
                    .iter()
                    .map(|f| f.name.as_str()),
            );
            // Include typed fields
            order.extend(type_def.fields.keys().map(|s| s.as_str()));
            order
        }),
        _ => None,
    };

    // Handle encryption scenarios
    if should_encrypt {
        let Some(key_id) = encrypt_config.get_key_id() else {
            return Ok((
                false,
                vec![ValidationError {
                    line: 0,
                    message: "encryption required but no key-id configured".to_string(),
                }],
                custom_info,
            ));
        };

        let existing_wrapped = doc
            .frontmatter
            .as_ref()
            .and_then(|fm| fm.wrapped_key.as_deref());

        match serialize_encrypted(
            &doc,
            field_order.as_deref(),
            &key_id,
            existing_wrapped,
            original_body_b64.as_deref(),
        )? {
            Some(formatted) => {
                fs::write(path, &formatted).context("Failed to write encrypted markdown")?;
                Ok((true, vec![], custom_info))
            }
            None => {
                // No change
                Ok((false, vec![], custom_info))
            }
        }
    } else {
        // Plain formatting (no encryption)
        let formatted = if let Some(ref order) = field_order {
            serialize_with_field_order(&doc, Some(order))
        } else {
            serialize(&doc)
        };

        if formatted != content {
            fs::write(path, &formatted).context("Failed to write formatted markdown")?;
            Ok((true, vec![], custom_info))
        } else {
            Ok((false, vec![], custom_info))
        }
    }
}

/// Determine if a file should be encrypted based on its type and config.
fn determine_should_encrypt(file_type: &FileType, config: &ProjectEncryptConfig) -> bool {
    match file_type {
        FileType::Journal => config.should_encrypt("journal"),
        FileType::Readme => config.should_encrypt("readme"),
        FileType::Claude => config.should_encrypt("claude"),
        FileType::Roadmap => config.should_encrypt("roadmap"),
        FileType::ClaudeCommand => false, // Never encrypt claude commands
        FileType::Custom { schema, type_name } => {
            // Check schema TypeDef for encrypted flag
            schema.get_type(type_name).is_some_and(|td| td.encrypted)
        }
    }
}

/// Extract year-month from a journal path (e.g., "2025-11" from "journal/2025-11.md").
fn extract_year_month(path: &Path) -> Option<String> {
    path.file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
}

/// Validate all schema directories and return loaded schemas.
fn collect_schemas(project_dir: &Path, result: &mut FormatResult) -> HashMap<PathBuf, Schema> {
    let mut schemas: HashMap<PathBuf, Schema> = HashMap::new();

    let walker = WalkDir::new(project_dir)
        .into_iter()
        .filter_entry(|e| e.file_name() != "node_modules");

    for entry in walker.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.file_name() == Some(std::ffi::OsStr::new(schema::SCHEMA_DIR)) && path.is_dir() {
            if schemas.contains_key(path) {
                continue;
            }

            match Schema::load(path) {
                Ok(schema) => {
                    let _ = schemas.insert(path.to_path_buf(), schema);
                }
                Err(e) => {
                    result.errors.push(FileError {
                        path: path.display().to_string(),
                        errors: vec![ValidationError {
                            line: 0,
                            message: format!("invalid schema: {}", e),
                        }],
                    });
                }
            }
        }
    }

    schemas
}

/// Format all markdown files for a project.
///
/// If `git_tree` is provided, link validation uses it instead of the filesystem.
/// This allows validating links to files not in the sparse checkout.
pub fn format_project(
    root: &Path,
    project: &str,
    git_tree: Option<&HashSet<String>>,
    opts: FormatOptions,
) -> Result<FormatResult> {
    let project_dir = root.join(PROJECTS_DIR).join(project);
    let mut result = FormatResult::default();

    // Load encryption config from project README
    let encrypt_config = ProjectEncryptConfig::load(&project_dir);

    // Collect and validate schemas
    let schemas = collect_schemas(&project_dir, &mut result);

    // Track documents by (schema_dir, type_name) for index generation
    let mut docs_by_type: HashMap<(PathBuf, String), Vec<PathBuf>> = HashMap::new();

    let walker = WalkDir::new(&project_dir).into_iter().filter_entry(|e| {
        // Skip node_modules directories
        e.file_name() != "node_modules"
    });

    for entry in walker.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().is_none_or(|e| e != "md") {
            continue;
        }

        let ctx = FormatContext {
            project,
            path,
            year_month: extract_year_month(path),
            git_tree,
            repo_root: Some(root),
        };

        result.files_checked += 1;
        match format_file(path, &ctx, &project_dir, &encrypt_config, &opts) {
            Ok((changed, errors, custom_info)) => {
                if changed {
                    result.files_formatted += 1;
                }
                if !errors.is_empty() {
                    result.errors.push(FileError {
                        path: path.display().to_string(),
                        errors,
                    });
                }
                // Track custom type documents for index generation
                if let Some(info) = custom_info {
                    docs_by_type
                        .entry((info.schema_dir, info.type_name))
                        .or_default()
                        .push(path.to_path_buf());
                }
            }
            Err(e) => {
                result.errors.push(FileError {
                    path: path.display().to_string(),
                    errors: vec![ValidationError {
                        line: 0,
                        message: e.to_string(),
                    }],
                });
            }
        }
    }

    // Generate indexes for schemas that have index paths defined
    generate_indexes(&schemas, &docs_by_type, &mut result);

    Ok(result)
}

/// Generate index files for schemas with index paths defined.
fn generate_indexes(
    schemas: &HashMap<PathBuf, Schema>,
    docs_by_type: &HashMap<(PathBuf, String), Vec<PathBuf>>,
    result: &mut FormatResult,
) {
    for (schema_dir, schema) in schemas {
        // Schema parent is directory containing .meow.d
        let Some(schema_parent) = schema_dir.parent() else {
            continue;
        };

        for (type_name, type_def) in &schema.types {
            let Some(index_path) = &type_def.index else {
                continue;
            };

            // Resolve index path relative to schema parent
            let full_index_path = schema_parent.join(index_path);

            // Get docs for this type (may be empty)
            let docs = docs_by_type
                .get(&(schema_dir.clone(), type_name.clone()))
                .map(|v| v.as_slice())
                .unwrap_or(&[]);

            // Generate index content
            let content = generate_index_content(type_name, &full_index_path, docs);

            // Read existing content to check if changed
            let existing = fs::read_to_string(&full_index_path).unwrap_or_default();
            if content != existing {
                if let Err(e) = fs::write(&full_index_path, &content) {
                    result.errors.push(FileError {
                        path: full_index_path.display().to_string(),
                        errors: vec![ValidationError {
                            line: 0,
                            message: format!("failed to write index: {}", e),
                        }],
                    });
                } else {
                    result.files_formatted += 1;
                }
            }
        }
    }
}

/// Generate markdown content for a type index.
fn generate_index_content(type_name: &str, index_path: &Path, docs: &[PathBuf]) -> String {
    let mut content = String::new();

    // Title: "{Type} Index" with first letter capitalized
    let title = format!(
        "{}{} Index",
        type_name.chars().next().unwrap_or_default().to_uppercase(),
        &type_name[1..]
    );
    content.push_str(&format!("# {title}\n"));

    if docs.is_empty() {
        return content;
    }

    content.push('\n');

    // Sort docs by filename
    let mut sorted_docs: Vec<_> = docs.iter().collect();
    sorted_docs.sort_by_key(|p| p.file_name());

    let index_dir = index_path.parent().unwrap_or(index_path);

    for doc in sorted_docs {
        // Compute relative path from index to doc
        let relative = pathdiff::diff_paths(doc, index_dir).unwrap_or_else(|| doc.clone());

        // Use filename without extension as link text
        let link_text = doc
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("Unknown");

        // URL-encode the path (spaces -> %20)
        let encoded_path = relative.to_string_lossy().replace(' ', "%20");

        content.push_str(&format!("- [{link_text}]({encoded_path})\n"));
    }

    content
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_index_content_empty() {
        let index_path = Path::new("/project/people/README.md");
        let content = generate_index_content("person", index_path, &[]);
        assert_eq!(content, "# Person Index\n");
    }

    #[test]
    fn test_generate_index_content_with_docs() {
        let index_path = Path::new("/project/people/README.md");
        let docs = vec![
            PathBuf::from("/project/people/Alice Smith.md"),
            PathBuf::from("/project/people/Bob.md"),
        ];
        let content = generate_index_content("person", index_path, &docs);
        assert_eq!(
            content,
            "# Person Index\n\n- [Alice Smith](Alice%20Smith.md)\n- [Bob](Bob.md)\n"
        );
    }

    #[test]
    fn test_generate_index_content_sorted() {
        let index_path = Path::new("/project/people/README.md");
        let docs = vec![
            PathBuf::from("/project/people/Zoe.md"),
            PathBuf::from("/project/people/Alice.md"),
            PathBuf::from("/project/people/Bob.md"),
        ];
        let content = generate_index_content("person", index_path, &docs);
        assert_eq!(
            content,
            "# Person Index\n\n- [Alice](Alice.md)\n- [Bob](Bob.md)\n- [Zoe](Zoe.md)\n"
        );
    }

    #[test]
    fn test_generate_index_content_subdirs() {
        let index_path = Path::new("/project/programmings/README.md");
        let docs = vec![
            PathBuf::from("/project/programmings/Rust.md"),
            PathBuf::from("/project/programmings/models/GPT4.md"),
        ];
        let content = generate_index_content("programming", index_path, &docs);
        assert_eq!(
            content,
            "# Programming Index\n\n- [GPT4](models/GPT4.md)\n- [Rust](Rust.md)\n"
        );
    }

    #[test]
    fn test_generate_index_capitalizes_type() {
        let index_path = Path::new("/project/test/README.md");
        let content = generate_index_content("hardware", index_path, &[]);
        assert_eq!(content, "# Hardware Index\n");
    }
}

//! Markdown file parsing, validation, and formatting.
//!
//! This module provides a markdown AST and enum-based dispatch for
//! different markdown file types (README, CLAUDE, journal).

mod command;
mod custom;
mod format;
mod parse;
pub mod schema;
mod style;

pub mod claude;
pub mod journal;
pub mod readme;
pub mod roadmap;

pub use format::{format_project, FormatOptions};
pub use journal::{
    read_journal, render_entries, render_timeline, CommitEntry, JournalEntry, TimelineItem,
};
pub use parse::{
    is_encrypted, parse, parse_encrypted, serialize, serialize_encrypted,
    serialize_with_field_order,
};
pub use style::skin;

use crate::JOURNAL_DIR;
use chrono::NaiveDate;
use serde::Deserialize;
use std::ffi::OsStr;
use std::path::Path;

/// A parsed markdown document.
#[derive(Debug, Clone)]
pub struct Document {
    pub frontmatter: Option<Frontmatter>,
    pub blocks: Vec<Block>,
}

/// YAML frontmatter fields.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct Frontmatter {
    pub created: Option<NaiveDate>,
    pub description: Option<String>,
    pub name: Option<String>,
    /// Document type for schema-driven validation.
    #[serde(rename = "type")]
    pub doc_type: Option<String>,
    /// Whether this file's body is encrypted.
    #[serde(default)]
    pub encrypted: bool,
    /// GPG key ID used for encryption (for encrypted files).
    #[serde(rename = "key-id")]
    pub key_id: Option<String>,
    /// Base64-encoded GPG-wrapped AES key (for encrypted files).
    #[serde(rename = "wrapped-key")]
    pub wrapped_key: Option<String>,
    /// Project-level encryption configuration (for README.md).
    #[serde(default)]
    pub encrypt: Option<EncryptConfig>,
    /// All other frontmatter fields (for schema-driven docs).
    #[serde(flatten)]
    pub extra: std::collections::HashMap<String, serde_yaml::Value>,
}

/// Project-level encryption configuration (stored in README.md frontmatter).
#[derive(Debug, Clone, Deserialize, Default)]
pub struct EncryptConfig {
    /// GPG key ID to use for encryption.
    #[serde(rename = "key-id")]
    pub key_id: Option<String>,
    /// File types that should be encrypted (e.g., ["journal"]).
    #[serde(default)]
    pub files: Vec<String>,
}

/// A block-level markdown element.
#[derive(Debug, Clone)]
pub enum Block {
    Heading {
        level: u8,
        content: Vec<Inline>,
    },
    Paragraph(Vec<Inline>),
    List {
        items: Vec<ListItem>,
        ordered: bool,
    },
    CodeBlock {
        language: Option<String>,
        content: String,
    },
    BlankLine,
}

/// A list item containing inline content and optional nested blocks.
#[derive(Debug, Clone)]
pub struct ListItem {
    pub content: Vec<Inline>,
    pub children: Vec<Block>,
}

/// An inline markdown element.
#[derive(Debug, Clone)]
pub enum Inline {
    Text(String),
    Strong(Vec<Inline>),
    Emphasis(Vec<Inline>),
    Link { text: String, url: String },
    Code(String),
    SoftBreak,
}

/// A validation error with line number.
#[derive(Debug)]
pub struct ValidationError {
    pub line: usize,
    pub message: String,
}

/// Result of formatting markdown files.
#[derive(Debug, Default)]
pub struct FormatResult {
    pub files_checked: usize,
    pub files_formatted: usize,
    pub errors: Vec<FileError>,
}

/// Errors for a single file.
#[derive(Debug)]
pub struct FileError {
    pub path: String,
    pub errors: Vec<ValidationError>,
}

/// Context passed to handlers.
#[derive(Debug)]
pub struct FormatContext<'a> {
    pub project: &'a str,
    pub path: &'a Path,
    pub year_month: Option<String>,
    /// Git tree paths for validating links (relative to repo root).
    pub git_tree: Option<&'a std::collections::HashSet<String>>,
    /// Repository root path for resolving relative paths.
    pub repo_root: Option<&'a Path>,
}

/// Type of markdown file for dispatch.
#[derive(Debug, Clone)]
pub enum FileType {
    Readme,
    Claude,
    ClaudeCommand,
    Journal,
    Roadmap,
    /// Schema-driven document type.
    Custom {
        schema: std::sync::Arc<schema::Schema>,
        type_name: String,
    },
}

impl FileType {
    /// Detect file type from path.
    ///
    /// If `project_root` is provided, also checks for schema-driven types.
    /// Hardcoded types (README, CLAUDE, ROADMAP, Journal) always take precedence.
    pub fn detect(path: &Path, project_root: Option<&Path>) -> Option<Self> {
        let filename = path.file_name()?;

        // Hardcoded types always win
        if filename == OsStr::new("README.md") {
            return Some(FileType::Readme);
        }
        if filename == OsStr::new("CLAUDE.md") {
            return Some(FileType::Claude);
        }
        if filename == OsStr::new("ROADMAP.md") {
            return Some(FileType::Roadmap);
        }
        // Journal: parent is "journal/" and file is .md
        if path.parent().and_then(|p| p.file_name()) == Some(OsStr::new(JOURNAL_DIR))
            && path.extension() == Some(OsStr::new("md"))
        {
            return Some(FileType::Journal);
        }
        // Claude command: grandparent is ".claude" and parent is "commands" and file is .md
        if let Some(parent) = path.parent() {
            if parent.file_name() == Some(OsStr::new("commands")) {
                if let Some(grandparent) = parent.parent() {
                    if grandparent.file_name() == Some(OsStr::new(".claude"))
                        && path.extension() == Some(OsStr::new("md"))
                    {
                        return Some(FileType::ClaudeCommand);
                    }
                }
            }
        }

        // Check for schema-driven type
        if let Some(root) = project_root {
            if let Some((schema, _schema_path)) = schema::Schema::find_for_path(path, root) {
                // Read frontmatter to get type field
                if let Ok(content) = std::fs::read_to_string(path) {
                    let doc = parse(&content);
                    if let Some(ref fm) = doc.frontmatter {
                        if let Some(ref type_name) = fm.doc_type {
                            if schema.types.contains_key(type_name) {
                                return Some(FileType::Custom {
                                    schema: std::sync::Arc::new(schema),
                                    type_name: type_name.clone(),
                                });
                            }
                        }
                    }
                }
            }
        }

        None
    }

    /// Validate the document.
    pub fn validate(&self, doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
        match self {
            FileType::Readme => readme::validate(doc, ctx),
            FileType::Claude => claude::validate(doc, ctx),
            FileType::ClaudeCommand => command::validate(doc, ctx),
            FileType::Journal => journal::validate(doc, ctx),
            FileType::Roadmap => roadmap::validate(doc, ctx),
            FileType::Custom { schema, type_name } => custom::validate(doc, ctx, schema, type_name),
        }
    }

    /// Normalize the document in place.
    pub fn normalize(&self, doc: &mut Document, ctx: &FormatContext) {
        match self {
            FileType::Readme => readme::normalize(doc, ctx),
            FileType::Claude => claude::normalize(doc, ctx),
            FileType::ClaudeCommand => command::normalize(doc, ctx),
            FileType::Journal => journal::normalize(doc, ctx),
            FileType::Roadmap => roadmap::normalize(doc, ctx),
            FileType::Custom { schema, type_name } => {
                custom::normalize(doc, ctx, schema, type_name);
            }
        }
    }
}

impl Block {
    /// Get the text content of a heading.
    pub fn heading_text(&self) -> Option<String> {
        match self {
            Block::Heading { content, .. } => Some(inlines_to_string(content)),
            _ => None,
        }
    }
}

/// Convert inline elements to a plain string.
pub fn inlines_to_string(inlines: &[Inline]) -> String {
    let mut result = String::new();
    for inline in inlines {
        match inline {
            Inline::Text(s) => result.push_str(s),
            Inline::Strong(inner) => result.push_str(&inlines_to_string(inner)),
            Inline::Emphasis(inner) => result.push_str(&inlines_to_string(inner)),
            Inline::Link { text, .. } => result.push_str(text),
            Inline::Code(s) => result.push_str(s),
            Inline::SoftBreak => result.push(' '),
        }
    }
    result
}

/// Convert inlines to markdown syntax (preserving link format etc).
pub fn inlines_to_markdown(inlines: &[Inline]) -> String {
    let mut result = String::new();
    for inline in inlines {
        match inline {
            Inline::Text(s) => result.push_str(s),
            Inline::Strong(inner) => {
                result.push_str("**");
                result.push_str(&inlines_to_markdown(inner));
                result.push_str("**");
            }
            Inline::Emphasis(inner) => {
                result.push('*');
                result.push_str(&inlines_to_markdown(inner));
                result.push('*');
            }
            Inline::Link { text, url } => {
                result.push('[');
                result.push_str(text);
                result.push_str("](");
                result.push_str(url);
                result.push(')');
            }
            Inline::Code(s) => {
                result.push('`');
                result.push_str(s);
                result.push('`');
            }
            Inline::SoftBreak => result.push(' '),
        }
    }
    result
}

/// Validate that a slice of blocks contains only bullet lists (no paragraphs or code blocks).
/// Returns an error for each non-list block found.
pub fn validate_bullets_only(blocks: &[Block], context: &str) -> Vec<ValidationError> {
    let mut errors = Vec::new();
    for block in blocks {
        match block {
            Block::List { .. } | Block::BlankLine | Block::Heading { .. } => {}
            Block::Paragraph(_) => {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("{context} expects bullet list, found paragraph"),
                });
            }
            Block::CodeBlock { .. } => {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("{context} expects bullet list, found code block"),
                });
            }
        }
    }
    errors
}

/// Validate that all links in the document are well-formed and local links exist.
pub fn validate_links(doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    fn check_inlines(inlines: &[Inline], ctx: &FormatContext, errors: &mut Vec<ValidationError>) {
        for inline in inlines {
            match inline {
                Inline::Link { url, .. } if url.is_empty() => {
                    errors.push(ValidationError {
                        line: 0,
                        message: "link has empty URL".to_string(),
                    });
                }
                Inline::Link { url, .. } => {
                    if let Some(err) = validate_link_exists(url, ctx) {
                        errors.push(err);
                    }
                }
                Inline::Strong(inner) | Inline::Emphasis(inner) => {
                    check_inlines(inner, ctx, errors);
                }
                _ => {}
            }
        }
    }

    fn check_blocks(blocks: &[Block], ctx: &FormatContext, errors: &mut Vec<ValidationError>) {
        for block in blocks {
            match block {
                Block::Heading { content, .. } => check_inlines(content, ctx, errors),
                Block::Paragraph(content) => check_inlines(content, ctx, errors),
                Block::List { items, .. } => {
                    for item in items {
                        check_inlines(&item.content, ctx, errors);
                        check_blocks(&item.children, ctx, errors);
                    }
                }
                Block::CodeBlock { .. } | Block::BlankLine => {}
            }
        }
    }

    check_blocks(&doc.blocks, ctx, &mut errors);

    errors
}

/// Check if a local link target exists.
pub fn validate_link_exists(link: &str, ctx: &FormatContext) -> Option<ValidationError> {
    // Skip external URLs
    if link.starts_with("http://") || link.starts_with("https://") {
        return None;
    }

    // Skip anchor-only links
    if link.starts_with('#') {
        return None;
    }

    // Check that relative links are properly URL-encoded (no literal spaces)
    if link.contains(' ') {
        return Some(ValidationError {
            line: 0,
            message: format!("link contains unencoded space: {link}"),
        });
    }

    // URL-decode the path (handles %20 for spaces, etc.)
    let decoded = urlencoding_decode(link);
    let link_path = std::path::Path::new(&decoded);

    // Resolve relative path against the file's directory
    let base_dir = ctx.path.parent()?;
    let resolved = base_dir.join(link_path);

    // Normalize the path (resolve .. and .)
    let normalized = normalize_path(&resolved);

    // Check against git tree if available, otherwise filesystem
    if let (Some(git_tree), Some(repo_root)) = (&ctx.git_tree, &ctx.repo_root) {
        // Convert absolute path to repo-relative path
        if let Ok(relative) = normalized.strip_prefix(repo_root) {
            let relative_str = relative.to_string_lossy().to_string();
            // Check if path is a file, or a directory containing tracked files
            let is_valid = git_tree.contains(&relative_str)
                || git_tree
                    .iter()
                    .any(|p| p.starts_with(&format!("{}/", relative_str)));
            if !is_valid && !resolved.exists() {
                return Some(ValidationError {
                    line: 0,
                    message: format!(
                        "link target not found: {} (checked git ls-tree HEAD:{} and stat {})",
                        link,
                        relative_str,
                        resolved.display()
                    ),
                });
            }
        } else {
            // Path is outside repo, can't validate via git
            return Some(ValidationError {
                line: 0,
                message: format!("link target outside repository: {}", link),
            });
        }
    } else if !resolved.exists() {
        return Some(ValidationError {
            line: 0,
            message: format!("link target not found: {}", link),
        });
    }

    None
}

/// Normalize a path by resolving . and .. components.
pub fn normalize_path(path: &Path) -> std::path::PathBuf {
    let mut result = std::path::PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                let _ = result.pop();
            }
            std::path::Component::CurDir => {}
            c => result.push(c),
        }
    }
    result
}

/// Simple URL decoding for file paths.
pub fn urlencoding_decode(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            // Try to read two hex digits
            let mut hex = String::new();
            if let Some(&c1) = chars.peek() {
                if c1.is_ascii_hexdigit() {
                    hex.push(chars.next().unwrap_or_default());
                    if let Some(&c2) = chars.peek() {
                        if c2.is_ascii_hexdigit() {
                            hex.push(chars.next().unwrap_or_default());
                        }
                    }
                }
            }
            if hex.len() == 2 {
                if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                    result.push(byte as char);
                    continue;
                }
            }
            // If we couldn't decode, keep the original
            result.push('%');
            result.push_str(&hex);
        } else {
            result.push(c);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_urlencoding_decode() {
        assert_eq!(urlencoding_decode("hello%20world"), "hello world");
        assert_eq!(urlencoding_decode("no%20spaces"), "no spaces");
        assert_eq!(urlencoding_decode("plain"), "plain");
        assert_eq!(urlencoding_decode("100%25"), "100%");
    }

    #[test]
    fn test_unencoded_space_rejected() {
        let ctx = FormatContext {
            project: "test",
            path: Path::new("/tmp/test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };
        let err = validate_link_exists("file with spaces.md", &ctx);
        assert!(err.is_some());
        assert!(err
            .unwrap()
            .message
            .contains("link contains unencoded space"));
    }

    #[test]
    fn test_encoded_space_accepted() {
        let ctx = FormatContext {
            project: "test",
            path: Path::new("/tmp/test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };
        // Encoded space should not trigger the unencoded space error
        // (it will fail on "target not found" instead)
        let err = validate_link_exists("file%20with%20spaces.md", &ctx);
        assert!(err.is_none() || !err.unwrap().message.contains("unencoded space"));
    }
}

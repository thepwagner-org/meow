//! Markdown file parsing, validation, and formatting.
//!
//! This module provides a markdown AST and enum-based dispatch for
//! different markdown file types (README, CLAUDE, journal).

mod command;
mod custom;
mod format;
mod parse;
pub mod schema;
mod skill;
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
    get_frontmatter_error, is_encrypted, parse, parse_encrypted, serialize, serialize_encrypted,
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
    /// 1-based line number for each block (parallel to `blocks`).
    /// Populated by the parser using pulldown-cmark byte offsets.
    pub block_lines: Vec<usize>,
}

/// YAML frontmatter fields.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct Frontmatter {
    #[serde(default, deserialize_with = "deserialize_date")]
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

/// Custom deserializer for `created` field that provides helpful error messages.
fn deserialize_date<'de, D>(deserializer: D) -> Result<Option<NaiveDate>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::Error;

    let value: Option<serde_yaml::Value> = Option::deserialize(deserializer)?;

    let Some(v) = value else {
        return Ok(None);
    };

    // If it's a string, try to parse as YYYY-MM-DD
    if let Some(s) = v.as_str() {
        return NaiveDate::parse_from_str(s, "%Y-%m-%d")
            .map(Some)
            .map_err(|_| {
                D::Error::custom(format!("created: expected date (YYYY-MM-DD), got '{s}'"))
            });
    }

    // YAML might parse "2026-01-01 10:51" as a timestamp - format it back
    // for a helpful error message
    let formatted = serde_yaml::to_string(&v)
        .unwrap_or_else(|_| format!("{v:?}"))
        .trim()
        .to_string();
    Err(D::Error::custom(format!(
        "created: expected date (YYYY-MM-DD), got '{formatted}'"
    )))
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
    BlockQuote(Vec<Block>),
    Table {
        alignments: Vec<ColumnAlignment>,
        header: Vec<Vec<Inline>>,
        rows: Vec<Vec<Vec<Inline>>>,
    },
    ThematicBreak,
    BlankLine,
}

/// Column alignment for table cells.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ColumnAlignment {
    None,
    Left,
    Center,
    Right,
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
    Strikethrough(Vec<Inline>),
    Link { text: String, url: String },
    Image { alt: String, url: String },
    Code(String),
    SoftBreak,
}

/// A validation error or fixable issue.
///
/// Each variant carries enough context to:
/// - Report a diagnostic (line number + message)
/// - Apply a fix if the issue is fixable
#[derive(Debug, Clone)]
pub enum ValidationError {
    // === Unfixable errors (require user action) ===
    /// Frontmatter is missing or failed to parse.
    MissingFrontmatter { reason: Option<String> },
    /// A required frontmatter field is missing.
    MissingRequiredField { line: usize, field: String },
    /// A field has an invalid type or value.
    InvalidFieldValue {
        line: usize,
        field: String,
        message: String,
    },
    /// A date string is in an invalid format.
    InvalidDateFormat { line: usize, text: String },
    /// A journal entry date doesn't match the file's year-month.
    DateFileMismatch {
        line: usize,
        date: String,
        expected_prefix: String,
    },
    /// A heading is malformed (e.g., missing space after ##).
    MalformedHeading { line: usize, text: String },
    /// A link target doesn't exist or is invalid.
    BrokenLink {
        line: usize,
        target: String,
        reason: String,
    },
    /// File is too large (warning).
    FileTooLarge {
        line: usize,
        size: usize,
        threshold: usize,
    },
    /// Schema-related error.
    SchemaError { line: usize, message: String },
    /// Section structure error (wrong order, unexpected section, etc).
    SectionError { line: usize, message: String },
    /// Missing required section.
    MissingSection { section: String },
    /// I/O error (file write failed, etc).
    IoError { line: usize, message: String },

    // === Fixable issues (normalize() handles these) ===
    /// H1 heading is missing; should be created with expected title.
    MissingH1 { expected: String },
    /// H1 heading doesn't match expected title.
    H1Mismatch { expected: String },
    /// Journal month H1 is missing.
    MissingMonthH1 { month_name: String },
    /// Legacy H3 date heading should be H2.
    LegacyH3Heading { date_text: String },
    /// Journal entries are out of order (should be newest-first).
    EntriesOutOfOrder {
        preamble: Vec<Block>,
        sorted_entries: Vec<(chrono::NaiveDate, Option<chrono::NaiveTime>, Vec<Block>)>,
    },
    /// Auto-managed section needs updating (e.g., Related Documents in CLAUDE.md).
    ManagedSectionNeedsUpdate {
        /// Index of existing section (if any), or None to append.
        section_start: Option<usize>,
        /// End index of section(s) to replace.
        section_end: usize,
        /// Template blocks to insert.
        template_blocks: Vec<Block>,
        /// Custom content to preserve after template.
        custom_content: Vec<Block>,
    },
    /// Section needs intro paragraph (e.g., Non-Goals in ROADMAP.md).
    SectionNeedsIntro {
        /// Index where intro should be inserted.
        insert_idx: usize,
        /// The intro text to insert.
        text: String,
    },
    /// Date/datetime field needs normalization.
    DateNeedsNormalization { field: String, normalized: String },
    /// Empty optional section should be removed.
    EmptyOptionalSection { section_indices: Vec<usize> },
}

impl ValidationError {
    /// Get the line number for this error (1-based, 0 = unknown).
    pub fn line(&self) -> usize {
        match self {
            Self::MissingFrontmatter { .. } => 1,
            Self::MissingRequiredField { line, .. } => *line,
            Self::InvalidFieldValue { line, .. } => *line,
            Self::InvalidDateFormat { line, .. } => *line,
            Self::DateFileMismatch { line, .. } => *line,
            Self::MalformedHeading { line, .. } => *line,
            Self::BrokenLink { line, .. } => *line,
            Self::FileTooLarge { line, .. } => *line,
            Self::SchemaError { line, .. } => *line,
            Self::SectionError { line, .. } => *line,
            Self::MissingSection { .. } => 1,
            Self::IoError { line, .. } => *line,
            // Fixable issues
            Self::MissingH1 { .. } => 1,
            Self::H1Mismatch { .. } => 1,
            Self::MissingMonthH1 { .. } => 1,
            Self::LegacyH3Heading { .. } => 1,
            Self::EntriesOutOfOrder { .. } => 1,
            Self::ManagedSectionNeedsUpdate { .. } => 1,
            Self::SectionNeedsIntro { .. } => 1,
            Self::DateNeedsNormalization { .. } => 1,
            Self::EmptyOptionalSection { .. } => 1,
        }
    }

    /// Get a human-readable message for this error.
    pub fn message(&self) -> String {
        let base = match self {
            Self::MissingFrontmatter { reason: None } => "missing frontmatter".to_string(),
            Self::MissingFrontmatter { reason: Some(r) } => format!("invalid frontmatter: {r}"),
            Self::MissingRequiredField { field, .. } => {
                format!("missing required field '{}'", field)
            }
            Self::InvalidFieldValue { field, message, .. } => {
                format!("field '{}': {}", field, message)
            }
            Self::InvalidDateFormat { text, .. } => {
                format!(
                    "invalid date format: {:?} (expected YYYY-MM-DD or YYYY-MM-DD HH:MM)",
                    text
                )
            }
            Self::DateFileMismatch {
                date,
                expected_prefix,
                ..
            } => {
                format!("date {} doesn't match file {}", date, expected_prefix)
            }
            Self::MalformedHeading { text, .. } => {
                format!("malformed heading: {:?} (missing space after ##)", text)
            }
            Self::BrokenLink { target, reason, .. } => {
                format!("broken link '{}': {}", target, reason)
            }
            Self::FileTooLarge {
                size, threshold, ..
            } => {
                format!(
                    "file is large ({} bytes, threshold: {}). Consider condensing.",
                    size, threshold
                )
            }
            Self::SchemaError { message, .. } => message.clone(),
            Self::SectionError { message, .. } => message.clone(),
            Self::MissingSection { section } => {
                format!("missing required section '{}'", section)
            }
            Self::IoError { message, .. } => message.clone(),
            // Fixable issues
            Self::MissingH1 { expected } => {
                format!("missing H1 heading (expected '{}')", expected)
            }
            Self::H1Mismatch { expected } => {
                format!("H1 heading should be '{}'", expected)
            }
            Self::MissingMonthH1 { month_name } => {
                format!("missing month heading (# {})", month_name)
            }
            Self::LegacyH3Heading { date_text } => {
                format!("legacy H3 heading '{}' should be H2", date_text)
            }
            Self::EntriesOutOfOrder { .. } => {
                "journal entries are out of order (should be newest first)".to_string()
            }
            Self::ManagedSectionNeedsUpdate { .. } => "managed section needs updating".to_string(),
            Self::SectionNeedsIntro { .. } => "section needs intro paragraph".to_string(),
            Self::DateNeedsNormalization { field, normalized } => {
                format!("field '{}' normalized to '{}'", field, normalized)
            }
            Self::EmptyOptionalSection { .. } => {
                "empty optional section(s) will be removed".to_string()
            }
        };

        if self.is_fixable() {
            format!("{base} (run meow fmt to fix)")
        } else {
            base
        }
    }

    /// Returns true if this issue can be automatically fixed.
    pub fn is_fixable(&self) -> bool {
        matches!(
            self,
            Self::MissingH1 { .. }
                | Self::H1Mismatch { .. }
                | Self::MissingMonthH1 { .. }
                | Self::LegacyH3Heading { .. }
                | Self::EntriesOutOfOrder { .. }
                | Self::ManagedSectionNeedsUpdate { .. }
                | Self::SectionNeedsIntro { .. }
                | Self::DateNeedsNormalization { .. }
                | Self::EmptyOptionalSection { .. }
        )
    }

    /// Apply the fix for this issue. No-op if not fixable.
    pub fn fix(&self, doc: &mut Document) {
        match self {
            Self::MissingH1 { expected } => {
                // Insert H1 at beginning
                doc.blocks.insert(
                    0,
                    Block::Heading {
                        level: 1,
                        content: vec![Inline::Text(expected.clone())],
                    },
                );
            }
            Self::H1Mismatch { expected } => {
                // Find and update H1
                if let Some(idx) = doc
                    .blocks
                    .iter()
                    .position(|b| matches!(b, Block::Heading { level: 1, .. }))
                {
                    doc.blocks[idx] = Block::Heading {
                        level: 1,
                        content: vec![Inline::Text(expected.clone())],
                    };
                }
            }
            Self::MissingMonthH1 { month_name } => {
                // Insert month H1 at beginning
                doc.blocks.insert(
                    0,
                    Block::Heading {
                        level: 1,
                        content: vec![Inline::Text(month_name.clone())],
                    },
                );
            }
            Self::LegacyH3Heading { date_text } => {
                // Find H3 with this text and convert to H2
                for block in &mut doc.blocks {
                    if let Block::Heading { level: 3, content } = block {
                        if inlines_to_string(content) == *date_text {
                            *block = Block::Heading {
                                level: 2,
                                content: content.clone(),
                            };
                        }
                    }
                }
            }
            Self::EntriesOutOfOrder {
                preamble,
                sorted_entries,
            } => {
                // Rebuild document with sorted entries
                doc.blocks = preamble.clone();
                for (_, _, blocks) in sorted_entries {
                    doc.blocks.extend(blocks.clone());
                }
            }
            Self::ManagedSectionNeedsUpdate {
                section_start,
                section_end,
                template_blocks,
                custom_content,
            } => {
                let mut new_section = template_blocks.clone();
                if !custom_content.is_empty() {
                    new_section.push(Block::BlankLine);
                    new_section.extend(custom_content.clone());
                }

                match section_start {
                    Some(idx) => {
                        let _ = doc.blocks.splice(*idx..*section_end, new_section);
                    }
                    None => {
                        // Append at end
                        if !doc.blocks.is_empty()
                            && !matches!(doc.blocks.last(), Some(Block::BlankLine))
                        {
                            doc.blocks.push(Block::BlankLine);
                        }
                        doc.blocks.extend(new_section);
                    }
                }
            }
            Self::SectionNeedsIntro { insert_idx, text } => {
                let intro = Block::Paragraph(vec![Inline::Text(text.clone())]);
                doc.blocks.insert(*insert_idx, intro);
            }
            Self::DateNeedsNormalization { field, normalized } => {
                if let Some(ref mut fm) = doc.frontmatter {
                    let _ = fm
                        .extra
                        .insert(field.clone(), serde_yaml::Value::String(normalized.clone()));
                }
            }
            Self::EmptyOptionalSection { section_indices } => {
                // Remove in reverse order to preserve indices
                for &idx in section_indices.iter().rev() {
                    if idx < doc.blocks.len() {
                        let _ = doc.blocks.remove(idx);
                    }
                }
            }
            // Unfixable errors - no-op
            _ => {}
        }
    }
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
    /// AGENTS.md - canonical agent instruction file (opencode and other agents).
    Agents,
    /// CLAUDE.md - agent instruction file for Claude Code (fallback).
    Claude,
    AgentCommand,
    /// Skill file (.opencode/skills/*/SKILL.md, .claude/skills/*/SKILL.md).
    AgentSkill,
    Journal,
    Roadmap,
    /// Schema-driven document type.
    Custom {
        schema: std::sync::Arc<schema::Schema>,
        type_name: String,
    },
    /// File is in a schema-covered directory but has no valid type field.
    UnknownSchemaType {
        schema: std::sync::Arc<schema::Schema>,
    },
}

impl FileType {
    /// Detect file type from path.
    ///
    /// If `project_root` is provided, also checks for schema-driven types.
    /// Hardcoded types (README, CLAUDE, ROADMAP, Journal) always take precedence.
    pub fn detect(path: &Path, project_root: Option<&Path>) -> Option<Self> {
        // Only process .md files
        if path.extension() != Some(OsStr::new("md")) {
            return None;
        }

        let filename = path.file_name()?;

        // Hardcoded types always win
        if filename == OsStr::new("README.md") {
            return Some(FileType::Readme);
        }
        if filename == OsStr::new("AGENTS.md") {
            return Some(FileType::Agents);
        }
        if filename == OsStr::new("CLAUDE.md") {
            return Some(FileType::Claude);
        }
        if filename == OsStr::new("ROADMAP.md") {
            return Some(FileType::Roadmap);
        }
        // Journal: parent is "journal/"
        if path.parent().and_then(|p| p.file_name()) == Some(OsStr::new(JOURNAL_DIR)) {
            return Some(FileType::Journal);
        }
        // Claude/opencode command: .claude/commands/*.md or .opencode/command/*.md
        if let Some(parent) = path.parent() {
            if let Some(grandparent) = parent.parent() {
                let parent_name = parent.file_name();
                let gp_name = grandparent.file_name();
                let is_claude_cmd = gp_name == Some(OsStr::new(".claude"))
                    && parent_name == Some(OsStr::new("commands"));
                let is_opencode_cmd = gp_name == Some(OsStr::new(".opencode"))
                    && parent_name == Some(OsStr::new("command"));
                if is_claude_cmd || is_opencode_cmd {
                    return Some(FileType::AgentCommand);
                }
            }
        }
        // Skill file: .opencode/skills/*/SKILL.md or .claude/skills/*/SKILL.md
        if filename == OsStr::new("SKILL.md") {
            if let Some(parent) = path.parent() {
                if let Some(grandparent) = parent.parent() {
                    if let Some(great_grandparent) = grandparent.parent() {
                        let gp_name = grandparent.file_name();
                        let ggp_name = great_grandparent.file_name();
                        if gp_name == Some(OsStr::new("skills"))
                            && (ggp_name == Some(OsStr::new(".opencode"))
                                || ggp_name == Some(OsStr::new(".claude")))
                        {
                            return Some(FileType::AgentSkill);
                        }
                    }
                }
            }
        }

        // Check for schema-driven type
        if let Some(root) = project_root {
            if let Some((schema, _schema_path)) = schema::Schema::find_for_path(path, root) {
                let schema = std::sync::Arc::new(schema);
                // Read frontmatter to get type field
                if let Ok(content) = std::fs::read_to_string(path) {
                    let doc = parse(&content);
                    if let Some(ref fm) = doc.frontmatter {
                        if let Some(ref type_name) = fm.doc_type {
                            // "none" explicitly opts out of all validation
                            if type_name == "none" {
                                return None;
                            }
                            if schema.types.contains_key(type_name) {
                                return Some(FileType::Custom {
                                    schema,
                                    type_name: type_name.clone(),
                                });
                            }
                        }
                    }
                }
                // File is under schema but has no valid type - report it
                return Some(FileType::UnknownSchemaType { schema });
            }
        }

        None
    }

    /// Validate the document.
    pub fn validate(&self, doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
        match self {
            FileType::Readme => readme::validate(doc, ctx),
            FileType::Agents => claude::validate(doc, ctx, "AGENTS.md"),
            FileType::Claude => claude::validate(doc, ctx, "CLAUDE.md"),
            FileType::AgentCommand => command::validate(doc, ctx),
            FileType::AgentSkill => skill::validate(doc, ctx),
            FileType::Journal => journal::validate(doc, ctx),
            FileType::Roadmap => roadmap::validate(doc, ctx),
            FileType::Custom { schema, type_name } => custom::validate(doc, ctx, schema, type_name),
            FileType::UnknownSchemaType { schema } => custom::validate_unknown_type(doc, schema),
        }
    }
}

/// Apply all fixable errors to a document.
pub fn apply_fixes(doc: &mut Document, errors: &[ValidationError]) {
    for error in errors {
        error.fix(doc);
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
            Inline::Strikethrough(inner) => result.push_str(&inlines_to_string(inner)),
            Inline::Link { text, .. } => result.push_str(text),
            Inline::Image { alt, .. } => result.push_str(alt),
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
            Inline::Strikethrough(inner) => {
                result.push_str("~~");
                result.push_str(&inlines_to_markdown(inner));
                result.push_str("~~");
            }
            Inline::Link { text, url } => {
                result.push('[');
                result.push_str(text);
                result.push_str("](");
                result.push_str(url);
                result.push(')');
            }
            Inline::Image { alt, url } => {
                result.push_str("![");
                result.push_str(alt);
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
///
/// `block_lines` provides 1-based line numbers parallel to `blocks` (may be empty).
pub fn validate_bullets_only(
    blocks: &[Block],
    block_lines: &[usize],
    context: &str,
) -> Vec<ValidationError> {
    let mut errors = Vec::new();
    for (i, block) in blocks.iter().enumerate() {
        let line = block_lines.get(i).copied().unwrap_or(0);
        match block {
            Block::List { .. } | Block::BlankLine | Block::Heading { .. } => {}
            Block::Paragraph(_) => {
                errors.push(ValidationError::SectionError {
                    line,
                    message: format!("{context} expects bullet list, found paragraph"),
                });
            }
            Block::CodeBlock { .. } => {
                errors.push(ValidationError::SectionError {
                    line,
                    message: format!("{context} expects bullet list, found code block"),
                });
            }
            Block::BlockQuote(_) => {
                errors.push(ValidationError::SectionError {
                    line,
                    message: format!("{context} expects bullet list, found block quote"),
                });
            }
            Block::ThematicBreak => {
                errors.push(ValidationError::SectionError {
                    line,
                    message: format!("{context} expects bullet list, found thematic break"),
                });
            }
            Block::Table { .. } => {
                errors.push(ValidationError::SectionError {
                    line,
                    message: format!("{context} expects bullet list, found table"),
                });
            }
        }
    }
    errors
}

/// Validate that all links in the document are well-formed and local links exist.
pub fn validate_links(doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    fn check_inlines(
        inlines: &[Inline],
        line: usize,
        ctx: &FormatContext,
        errors: &mut Vec<ValidationError>,
    ) {
        for inline in inlines {
            match inline {
                Inline::Link { url, .. } | Inline::Image { url, .. } if url.is_empty() => {
                    errors.push(ValidationError::BrokenLink {
                        line,
                        target: String::new(),
                        reason: "link has empty URL".to_string(),
                    });
                }
                Inline::Link { url, .. } | Inline::Image { url, .. } => {
                    if let Some(err) = validate_link_exists(url, line, ctx) {
                        errors.push(err);
                    }
                }
                Inline::Strong(inner) | Inline::Emphasis(inner) | Inline::Strikethrough(inner) => {
                    check_inlines(inner, line, ctx, errors);
                }
                _ => {}
            }
        }
    }

    fn check_blocks(
        blocks: &[Block],
        block_lines: &[usize],
        ctx: &FormatContext,
        errors: &mut Vec<ValidationError>,
    ) {
        for (i, block) in blocks.iter().enumerate() {
            let line = block_lines.get(i).copied().unwrap_or(0);
            match block {
                Block::Heading { content, .. } => check_inlines(content, line, ctx, errors),
                Block::Paragraph(content) => check_inlines(content, line, ctx, errors),
                Block::List { items, .. } => {
                    for item in items {
                        check_inlines(&item.content, line, ctx, errors);
                        // Nested blocks inherit the parent list block's line
                        let child_lines = vec![line; item.children.len()];
                        check_blocks(&item.children, &child_lines, ctx, errors);
                    }
                }
                Block::CodeBlock { .. } | Block::BlankLine | Block::ThematicBreak => {}
                Block::BlockQuote(inner) => {
                    let child_lines = vec![line; inner.len()];
                    check_blocks(inner, &child_lines, ctx, errors);
                }
                Block::Table { header, rows, .. } => {
                    for cell in header {
                        check_inlines(cell, line, ctx, errors);
                    }
                    for row in rows {
                        for cell in row {
                            check_inlines(cell, line, ctx, errors);
                        }
                    }
                }
            }
        }
    }

    check_blocks(&doc.blocks, &doc.block_lines, ctx, &mut errors);

    errors
}

/// Check if a local link target exists.
pub fn validate_link_exists(
    link: &str,
    line: usize,
    ctx: &FormatContext,
) -> Option<ValidationError> {
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
        return Some(ValidationError::BrokenLink {
            line,
            target: link.to_string(),
            reason: "link contains unencoded space".to_string(),
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
                return Some(ValidationError::BrokenLink {
                    line,
                    target: link.to_string(),
                    reason: format!(
                        "not found (checked git ls-tree HEAD:{} and stat {})",
                        relative_str,
                        resolved.display()
                    ),
                });
            }
        } else {
            // Path is outside repo, can't validate via git
            return Some(ValidationError::BrokenLink {
                line,
                target: link.to_string(),
                reason: "target outside repository".to_string(),
            });
        }
    } else if !resolved.exists() {
        return Some(ValidationError::BrokenLink {
            line,
            target: link.to_string(),
            reason: "target not found".to_string(),
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

/// Detect link-like patterns that failed to parse due to whitespace in URL.
///
/// CommonMark parsers don't recognize `[text](url with space)` as a link,
/// so we scan the raw text to catch these malformed patterns.
///
/// Accepts full markdown content including frontmatter.
pub fn detect_malformed_links(content: &str) -> Vec<ValidationError> {
    use regex::Regex;

    let mut errors = Vec::new();

    // Pattern: [text](url containing space or tab)
    // This matches link-like syntax that would fail to parse due to whitespace
    let Ok(re) = Regex::new(r"\[([^\]]+)\]\(([^)]*[ \t][^)]*)\)") else {
        return errors;
    };

    // Track state
    let mut in_frontmatter = false;
    let mut seen_frontmatter_start = false;
    let mut in_fenced_code = false;
    let mut current_line = 1; // 1-based

    for line in content.lines() {
        // Handle frontmatter (YAML between --- markers)
        if line == "---" {
            if !seen_frontmatter_start && current_line == 1 {
                in_frontmatter = true;
                seen_frontmatter_start = true;
                current_line += 1;
                continue;
            } else if in_frontmatter {
                in_frontmatter = false;
                current_line += 1;
                continue;
            }
        }

        // Skip frontmatter content
        if in_frontmatter {
            current_line += 1;
            continue;
        }

        // Check for fenced code block markers
        if line.trim_start().starts_with("```") {
            in_fenced_code = !in_fenced_code;
            current_line += 1;
            continue;
        }

        // Skip content inside fenced code blocks
        if in_fenced_code {
            current_line += 1;
            continue;
        }

        // Check for malformed links on this line
        for cap in re.captures_iter(line) {
            if let Some(url_match) = cap.get(2) {
                let url = url_match.as_str();
                // Skip if this looks like it's inside inline code
                // Simple heuristic: check if there's a backtick before the match
                let prefix = &line[..cap.get(0).map(|m| m.start()).unwrap_or(0)];
                let backtick_count = prefix.chars().filter(|&c| c == '`').count();
                if backtick_count % 2 == 1 {
                    // Odd number of backticks = we're inside inline code
                    continue;
                }

                errors.push(ValidationError::BrokenLink {
                    line: current_line,
                    target: url.to_string(),
                    reason: "link contains unencoded space (use %20 instead)".to_string(),
                });
            }
        }

        current_line += 1;
    }

    errors
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
        let err = validate_link_exists("file with spaces.md", 1, &ctx);
        assert!(err.is_some());
        assert!(err.unwrap().message().contains("unencoded space"));
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
        let err = validate_link_exists("file%20with%20spaces.md", 1, &ctx);
        assert!(err.is_none() || !err.unwrap().message().contains("unencoded space"));
    }

    #[test]
    fn test_detect_malformed_link_with_space() {
        let content = "# Title\n\nSee [my link](file with space.md) here.";
        let errors = detect_malformed_links(content);
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message().contains("unencoded space"));
        assert_eq!(errors[0].line(), 3);
    }

    #[test]
    fn test_detect_malformed_link_skips_code_block() {
        let content = "# Title\n\n```\n[not a link](has spaces.md)\n```\n\nText.";
        let errors = detect_malformed_links(content);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_detect_malformed_link_skips_inline_code() {
        let content = "# Title\n\nSee `[not a link](has spaces.md)` here.";
        let errors = detect_malformed_links(content);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_detect_malformed_link_valid_link_ignored() {
        let content = "# Title\n\nSee [my link](valid-file.md) here.";
        let errors = detect_malformed_links(content);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_detect_malformed_link_skips_frontmatter() {
        let content =
            "---\ncreated: 2024-01-01\n---\n# Title\n\nSee [my link](file with space.md) here.";
        let errors = detect_malformed_links(content);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].line(), 6); // After 3 lines of frontmatter + blank + title
    }
}

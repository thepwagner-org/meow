//! Schema definitions for config-driven markdown validation.
//!
//! Schemas are loaded from `.meow.d/` directories containing type files.
//! Each `{type}.yaml` file defines a document type.

use anyhow::{Context, Result};
use serde::Deserialize;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Schema directory name.
pub const SCHEMA_DIR: &str = ".meow.d";

/// A schema defining document types for a collection.
#[derive(Debug, Clone, Default)]
pub struct Schema {
    /// Document type definitions.
    pub types: HashMap<String, TypeDef>,
}

/// Definition of a document type.
#[derive(Debug, Clone, Deserialize)]
pub struct TypeDef {
    /// Human-readable description.
    #[serde(default)]
    pub description: Option<String>,
    /// Whether files of this type should be encrypted.
    #[serde(default)]
    pub encrypted: bool,
    /// Field definitions (order matters for serialization).
    #[serde(default)]
    pub fields: indexmap::IndexMap<String, FieldDef>,
    /// Document structure rules.
    #[serde(default)]
    pub structure: StructureDef,
    /// Path to index file (relative to schema parent dir).
    #[serde(default)]
    pub index: Option<String>,
}

impl TypeDef {
    /// Validate the type definition is internally consistent.
    pub fn validate(&self, type_name: &str) -> Result<()> {
        for (field_name, field_def) in &self.fields {
            field_def
                .validate(field_name)
                .with_context(|| format!("in type '{}'", type_name))?;
        }
        Ok(())
    }
}

/// Document structure definition.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct StructureDef {
    /// Title (H1) must match filename (without .md extension).
    #[serde(default)]
    pub title_from_filename: bool,
    /// Frontmatter field definitions (order matters for serialization).
    #[serde(default)]
    pub frontmatter: Vec<FrontmatterFieldDef>,
    /// Intro section (content between H1 and first H2).
    #[serde(default)]
    pub intro: Option<SectionDef>,
    /// Allowed sections in order. Sections not in this list are disallowed.
    /// If empty, any sections are allowed.
    #[serde(default)]
    pub sections: Vec<SectionDef>,
}

/// Definition of a document section.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct SectionDef {
    /// Section heading text (ignored for intro).
    #[serde(default)]
    pub title: String,
    /// Description for LLM guidance.
    #[serde(default)]
    pub description: Option<String>,
    /// Allow paragraph content (default: false, bullets required).
    #[serde(default)]
    pub paragraph: bool,
    /// Whether this section is required.
    #[serde(default)]
    pub required: bool,
    /// Template showing expected format (for LLM reference).
    #[serde(default)]
    pub template: Option<String>,
    /// Link constraints for this section.
    #[serde(default)]
    pub links: Option<LinksDef>,
}

/// Link constraints for a section.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct LinksDef {
    /// Target type name - links must point to files of this type.
    pub target_type: Option<String>,
    /// Whether links must be bidirectional (target links back).
    #[serde(default)]
    pub bidirectional: bool,
}

/// Definition of a frontmatter field in structure.frontmatter.
///
/// This is a simpler format than `FieldDef`, used for declaring required
/// frontmatter fields without type validation.
#[derive(Debug, Clone, Deserialize)]
pub struct FrontmatterFieldDef {
    /// Field name.
    pub name: String,
    /// Description for documentation/LLM guidance.
    #[serde(default)]
    pub description: Option<String>,
    /// Whether the field is required.
    #[serde(default)]
    pub required: bool,
}

/// Definition of a frontmatter field.
#[derive(Debug, Clone, Deserialize)]
pub struct FieldDef {
    /// The field type.
    #[serde(rename = "type")]
    pub field_type: FieldType,
    /// Whether the field is required.
    #[serde(default)]
    pub required: bool,
    /// Valid values for enum fields.
    #[serde(default)]
    pub values: Option<Vec<String>>,
    /// Item type for list fields.
    #[serde(default)]
    pub item_type: Option<FieldType>,
}

impl FieldDef {
    /// Validate the field definition is internally consistent.
    ///
    /// Checks:
    /// - Enum fields must have non-empty `values`
    /// - List fields with enum item_type must have non-empty `values`
    pub fn validate(&self, field_name: &str) -> Result<()> {
        match self.field_type {
            FieldType::Enum => {
                if self.values.as_ref().is_none_or(|v| v.is_empty()) {
                    anyhow::bail!(
                        "field '{}': enum type requires non-empty 'values'",
                        field_name
                    );
                }
            }
            FieldType::List => {
                if self.item_type == Some(FieldType::Enum)
                    && self.values.as_ref().is_none_or(|v| v.is_empty())
                {
                    anyhow::bail!(
                        "field '{}': list of enum requires non-empty 'values'",
                        field_name
                    );
                }
            }
            _ => {}
        }
        Ok(())
    }
}

/// Types of frontmatter fields.
#[derive(Debug, Clone, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum FieldType {
    /// Any text value.
    String,
    /// Date only, normalized to YYYY-MM-DD.
    Date,
    /// Date with time, normalized to ISO 8601 with timezone.
    Datetime,
    /// Whole number.
    Integer,
    /// Boolean true/false.
    Bool,
    /// One of a fixed set of values.
    Enum,
    /// URL or relative file path.
    Link,
    /// Array of items.
    List,
}

/// A segment of a parsed template.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateSegment {
    /// Literal text that must match exactly.
    Literal(String),
    /// A markdown link: [text](url)
    Link,
    /// A date: YYYY-MM-DD
    Date,
    /// Free text (matches anything).
    Text,
}

/// Parse a template string into segments.
///
/// Detects patterns:
/// - `[...](...) → Link
/// - `YYYY-MM-DD` → Date
/// - Separators and punctuation → Literal
/// - Other placeholder text → Text
pub fn parse_template(template: &str) -> Vec<TemplateSegment> {
    let mut segments = Vec::new();
    let mut remaining = template;

    while !remaining.is_empty() {
        // Try to match link pattern: [...](...)
        if remaining.starts_with('[') {
            if let Some(end) = find_link_end(remaining) {
                segments.push(TemplateSegment::Link);
                remaining = &remaining[end..];
                continue;
            }
        }

        // Try to match date pattern: YYYY-MM-DD (or placeholder like "YYYY-MM-DD")
        if let Some(end) = match_date_pattern(remaining) {
            segments.push(TemplateSegment::Date);
            remaining = &remaining[end..];
            continue;
        }

        // Try to match separator: " - ", ", ", "; ", ": ", etc.
        if let Some(end) = match_separator(remaining) {
            segments.push(TemplateSegment::Literal(remaining[..end].to_string()));
            remaining = &remaining[end..];
            continue;
        }

        // Match text until next pattern or separator
        let end = find_text_end(remaining);
        if end > 0 {
            segments.push(TemplateSegment::Text);
            remaining = &remaining[end..];
        } else {
            // Single character as literal
            let c = remaining.chars().next().unwrap_or(' ');
            segments.push(TemplateSegment::Literal(c.to_string()));
            remaining = &remaining[c.len_utf8()..];
        }
    }

    segments
}

/// Find the end of a markdown link pattern [text](url).
fn find_link_end(s: &str) -> Option<usize> {
    if !s.starts_with('[') {
        return None;
    }

    let mut depth = 0;
    let mut in_url = false;
    let mut chars = s.char_indices();

    while let Some((i, c)) = chars.next() {
        match c {
            '[' if !in_url => depth += 1,
            ']' if !in_url => {
                depth -= 1;
                if depth == 0 {
                    // Expect (
                    if let Some((_, '(')) = chars.next() {
                        in_url = true;
                    } else {
                        return None;
                    }
                }
            }
            ')' if in_url => {
                return Some(i + 1);
            }
            _ => {}
        }
    }

    None
}

/// Match a date pattern like YYYY-MM-DD or placeholder text.
fn match_date_pattern(s: &str) -> Option<usize> {
    // Match literal "YYYY-MM-DD"
    if s.starts_with("YYYY-MM-DD") {
        return Some(10);
    }

    // Match actual date like 2024-03-15
    if s.len() >= 10 {
        let bytes = s.as_bytes();
        if bytes[0].is_ascii_digit()
            && bytes[1].is_ascii_digit()
            && bytes[2].is_ascii_digit()
            && bytes[3].is_ascii_digit()
            && bytes[4] == b'-'
            && bytes[5].is_ascii_digit()
            && bytes[6].is_ascii_digit()
            && bytes[7] == b'-'
            && bytes[8].is_ascii_digit()
            && bytes[9].is_ascii_digit()
        {
            return Some(10);
        }
    }

    None
}

/// Match a separator pattern.
fn match_separator(s: &str) -> Option<usize> {
    // Common separators with optional surrounding spaces
    let separators = [" - ", ", ", "; ", ": ", " (", ") ", "(", ")", " – ", "–"];

    for sep in separators {
        if s.starts_with(sep) {
            return Some(sep.len());
        }
    }

    None
}

/// Find the end of a text segment (until next pattern or separator).
fn find_text_end(s: &str) -> usize {
    for (i, c) in s.char_indices() {
        // Stop at link start
        if c == '[' {
            return i;
        }
        // Stop at separator start
        if c == '-' || c == ',' || c == ';' || c == ':' || c == '(' || c == ')' {
            // Check if it's actually a separator
            if match_separator(&s[i..]).is_some() {
                return i;
            }
        }
        // Stop at date pattern
        if c.is_ascii_digit() && match_date_pattern(&s[i..]).is_some() {
            return i;
        }
    }

    s.len()
}

/// Check if a string matches the template segments.
pub fn matches_template(s: &str, segments: &[TemplateSegment]) -> bool {
    matches_template_recursive(s, segments)
}

/// Recursive template matching.
fn matches_template_recursive(s: &str, segments: &[TemplateSegment]) -> bool {
    if segments.is_empty() {
        return s.trim().is_empty();
    }

    let segment = &segments[0];
    let rest = &segments[1..];

    match segment {
        TemplateSegment::Literal(lit) => {
            if s.starts_with(lit.as_str()) {
                matches_template_recursive(&s[lit.len()..], rest)
            } else {
                false
            }
        }
        TemplateSegment::Link => {
            if let Some(end) = find_link_end(s) {
                matches_template_recursive(&s[end..], rest)
            } else {
                false
            }
        }
        TemplateSegment::Date => {
            if let Some(end) = match_date_pattern(s) {
                matches_template_recursive(&s[end..], rest)
            } else {
                false
            }
        }
        TemplateSegment::Text => {
            // Text matches any characters until the next segment can match
            // Try progressively longer matches
            for end in 0..=s.len() {
                if matches_template_recursive(&s[end..], rest) {
                    return true;
                }
            }
            false
        }
    }
}

impl Schema {
    /// Load a schema from a `.meow.d/` directory.
    ///
    /// Each `{type}.yaml` file in the directory defines a document type.
    pub fn load(dir: &Path) -> Result<Self> {
        let mut schema = Schema::default();

        for entry in std::fs::read_dir(dir)
            .with_context(|| format!("failed to read schema dir: {}", dir.display()))?
        {
            let entry = entry?;
            let path = entry.path();

            // Skip non-yaml files
            if path.extension().is_none_or(|e| e != "yaml" && e != "yml") {
                continue;
            }

            // Type name is the file stem
            let Some(type_name) = path.file_stem().and_then(|s| s.to_str()) else {
                continue;
            };

            let content = std::fs::read_to_string(&path)
                .with_context(|| format!("failed to read type file: {}", path.display()))?;
            let type_def: TypeDef = serde_yaml::from_str(&content)
                .with_context(|| format!("failed to parse type file: {}", path.display()))?;

            type_def
                .validate(type_name)
                .with_context(|| format!("invalid type file: {}", path.display()))?;

            let _ = schema.types.insert(type_name.to_string(), type_def);
        }

        Ok(schema)
    }

    /// Find a schema directory by walking up from the given path.
    ///
    /// Searches for `.meow.d/` in parent directories up to (and including) `root`.
    /// Returns the schema and its location if found.
    ///
    /// Note: This silently skips invalid schemas. Use `find_for_path_strict` to get errors.
    pub fn find_for_path(path: &Path, root: &Path) -> Option<(Self, PathBuf)> {
        Self::find_for_path_strict(path, root).unwrap_or_default()
    }

    /// Find a schema directory, returning errors for invalid schemas.
    ///
    /// Returns:
    /// - `Ok(Some((schema, path)))` if a valid schema was found
    /// - `Ok(None)` if no schema directory exists
    /// - `Err(...)` if a schema directory exists but is invalid
    pub fn find_for_path_strict(path: &Path, root: &Path) -> Result<Option<(Self, PathBuf)>> {
        let Some(mut search_dir) = path.parent() else {
            return Ok(None);
        };

        loop {
            let schema_dir = search_dir.join(SCHEMA_DIR);
            if schema_dir.is_dir() {
                let schema = Self::load(&schema_dir)?;
                return Ok(Some((schema, schema_dir)));
            }

            // Stop at root
            if search_dir == root {
                break;
            }

            // Walk up
            let Some(parent) = search_dir.parent() else {
                break;
            };
            search_dir = parent;
        }

        Ok(None)
    }

    /// Get a type definition by name.
    pub fn get_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_person_type() {
        let yaml = r#"
description: "A person reference document"
fields:
  name:
    type: string
    required: true
  born:
    type: date
    required: false
  occupation:
    type: string
"#;
        let person: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert_eq!(
            person.description.as_deref(),
            Some("A person reference document")
        );
        assert_eq!(person.fields.len(), 3);

        let name_field = person.fields.get("name").expect("should have name field");
        assert_eq!(name_field.field_type, FieldType::String);
        assert!(name_field.required);

        let born_field = person.fields.get("born").expect("should have born field");
        assert_eq!(born_field.field_type, FieldType::Date);
        assert!(!born_field.required);
    }

    #[test]
    fn test_parse_tvshow_type() {
        let yaml = r#"
fields:
  title:
    type: string
    required: true
  status:
    type: enum
    values: [watching, completed, dropped, planned]
    required: true
  rating:
    type: integer
"#;
        let tvshow: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        let status_field = tvshow.fields.get("status").expect("should have status");
        assert_eq!(status_field.field_type, FieldType::Enum);
        assert_eq!(status_field.values.as_ref().map(|v| v.len()), Some(4));
    }

    #[test]
    fn test_field_order_preserved() {
        let yaml = r#"
fields:
  zebra:
    type: string
  alpha:
    type: string
  middle:
    type: string
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");

        let keys: Vec<&String> = type_def.fields.keys().collect();
        assert_eq!(keys, vec!["zebra", "alpha", "middle"]);
    }

    #[test]
    fn test_datetime_field() {
        let yaml = r#"
fields:
  created:
    type: datetime
    required: true
  date:
    type: date
"#;
        let event: TypeDef = serde_yaml::from_str(yaml).expect("should parse");

        let created = event.fields.get("created").expect("should have created");
        assert_eq!(created.field_type, FieldType::Datetime);

        let date = event.fields.get("date").expect("should have date");
        assert_eq!(date.field_type, FieldType::Date);
    }

    #[test]
    fn test_list_field() {
        let yaml = r#"
fields:
  tags:
    type: list
    item_type: string
  links:
    type: list
    item_type: link
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");

        let tags = type_def.fields.get("tags").expect("should have tags");
        assert_eq!(tags.field_type, FieldType::List);
        assert_eq!(tags.item_type, Some(FieldType::String));
    }

    #[test]
    fn test_index_field() {
        let yaml = r#"
description: "A person reference document"
index: people/README.md
fields:
  name:
    type: string
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert_eq!(type_def.index.as_deref(), Some("people/README.md"));
    }

    #[test]
    fn test_index_field_optional() {
        let yaml = r#"
description: "A document without index"
fields:
  name:
    type: string
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert!(type_def.index.is_none());
    }

    #[test]
    fn test_parse_structure_frontmatter() {
        let yaml = r#"
description: "A target asset"
structure:
  title_from_filename: true
  frontmatter:
    - name: category
      description: "account | device | physical"
      required: true
    - name: priority
      required: false
  sections:
    - title: Notes
      optional: true
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");

        assert_eq!(type_def.structure.frontmatter.len(), 2);

        let category = &type_def.structure.frontmatter[0];
        assert_eq!(category.name, "category");
        assert_eq!(
            category.description.as_deref(),
            Some("account | device | physical")
        );
        assert!(category.required);

        let priority = &type_def.structure.frontmatter[1];
        assert_eq!(priority.name, "priority");
        assert!(priority.description.is_none());
        assert!(!priority.required);
    }

    #[test]
    fn test_structure_frontmatter_order_preserved() {
        let yaml = r#"
structure:
  frontmatter:
    - name: zebra
    - name: alpha
    - name: middle
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");

        let names: Vec<&str> = type_def
            .structure
            .frontmatter
            .iter()
            .map(|f| f.name.as_str())
            .collect();
        assert_eq!(names, vec!["zebra", "alpha", "middle"]);
    }

    #[test]
    fn test_structure_frontmatter_empty_by_default() {
        let yaml = r#"
description: "A simple type"
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert!(type_def.structure.frontmatter.is_empty());
    }

    #[test]
    fn test_enum_requires_values() {
        let yaml = r#"
fields:
  status:
    type: enum
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        let result = type_def.validate("test");
        let err = result.expect_err("should fail without values");
        // Use debug format to see full error chain
        let msg = format!("{:?}", err);
        assert!(
            msg.contains("enum type requires non-empty 'values'"),
            "expected enum error, got: {}",
            msg
        );
    }

    #[test]
    fn test_enum_with_empty_values_fails() {
        let yaml = r#"
fields:
  status:
    type: enum
    values: []
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        let result = type_def.validate("test");
        assert!(result.is_err());
    }

    #[test]
    fn test_enum_with_values_succeeds() {
        let yaml = r#"
fields:
  status:
    type: enum
    values: [active, inactive]
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert!(type_def.validate("test").is_ok());
    }

    #[test]
    fn test_list_of_enum_requires_values() {
        let yaml = r#"
fields:
  tags:
    type: list
    item_type: enum
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        let result = type_def.validate("test");
        let err = result.expect_err("should fail without values");
        // Use debug format to see full error chain
        let msg = format!("{:?}", err);
        assert!(
            msg.contains("list of enum requires non-empty 'values'"),
            "expected list-of-enum error, got: {}",
            msg
        );
    }

    #[test]
    fn test_list_of_enum_with_values_succeeds() {
        let yaml = r#"
fields:
  tags:
    type: list
    item_type: enum
    values: [work, personal, health]
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert!(type_def.validate("test").is_ok());
    }

    #[test]
    fn test_list_of_string_does_not_require_values() {
        let yaml = r#"
fields:
  names:
    type: list
    item_type: string
"#;
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("should parse");
        assert!(type_def.validate("test").is_ok());
    }
}

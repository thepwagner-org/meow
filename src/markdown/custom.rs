//! Schema-driven markdown validation and normalization.
//!
//! This module handles documents that are validated against `.meow.d/` definitions.

use super::{
    schema::{FieldType, Schema, TypeDef},
    validate_bullets_only, validate_link_exists, Block, Document, FormatContext, ValidationError,
};
use chrono::NaiveDate;

/// Validate a document against its schema type definition.
pub fn validate(
    doc: &Document,
    ctx: &FormatContext,
    schema: &Schema,
    type_name: &str,
) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    let Some(type_def) = schema.get_type(type_name) else {
        errors.push(ValidationError {
            line: 0,
            message: format!("unknown type '{}' in schema", type_name),
        });
        return errors;
    };

    let Some(ref fm) = doc.frontmatter else {
        errors.push(ValidationError {
            line: 0,
            message: "missing frontmatter".to_string(),
        });
        return errors;
    };

    // Validate type field matches schema type name
    if let Some(ref doc_type) = fm.doc_type {
        if doc_type != type_name {
            errors.push(ValidationError {
                line: 0,
                message: format!(
                    "type '{}' does not match schema type '{}'",
                    doc_type, type_name
                ),
            });
        }
    } else {
        errors.push(ValidationError {
            line: 0,
            message: format!("missing required field 'type' (expected '{}')", type_name),
        });
    }

    validate_fields(fm, type_def, &mut errors);
    validate_frontmatter_links(fm, type_def, ctx, &mut errors);
    validate_structure(doc, ctx, type_def, &mut errors);
    errors
}

/// Validate document structure against type definition.
fn validate_structure(
    doc: &Document,
    ctx: &FormatContext,
    type_def: &TypeDef,
    errors: &mut Vec<ValidationError>,
) {
    let structure = &type_def.structure;

    // Find H1
    let h1 = doc.blocks.iter().find_map(|b| match b {
        Block::Heading { level: 1, content } => Some(super::inlines_to_string(content)),
        _ => None,
    });

    // Validate title matches filename
    if structure.title_from_filename {
        let expected_title = ctx.path.file_stem().and_then(|s| s.to_str()).unwrap_or("");

        match &h1 {
            Some(title) if title == expected_title => {}
            Some(title) => {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("title '{}' must match filename '{}'", title, expected_title),
                });
            }
            None => {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("missing title (expected '{}')", expected_title),
                });
            }
        }
    }

    // Validate intro section (content between H1 and first H2)
    if let Some(ref intro_def) = structure.intro {
        validate_intro_content(doc, intro_def, errors);
    }

    // Validate sections order
    if !structure.sections.is_empty() {
        let allowed_titles: Vec<&str> = structure
            .sections
            .iter()
            .map(|s| s.title.as_str())
            .collect();

        let h2s: Vec<String> = doc
            .blocks
            .iter()
            .filter_map(|b| match b {
                Block::Heading { level: 2, content } => Some(super::inlines_to_string(content)),
                _ => None,
            })
            .collect();

        // Check each H2 is in the allowed list
        for h2 in &h2s {
            if !allowed_titles.contains(&h2.as_str()) {
                errors.push(ValidationError {
                    line: 0,
                    message: format!(
                        "unexpected section '{}' (allowed: {})",
                        h2,
                        allowed_titles.join(", ")
                    ),
                });
            }
        }

        // Check ordering: for each pair of sections that both appear, first must come before second
        let mut last_allowed_idx = 0;
        for h2 in &h2s {
            if let Some(idx) = allowed_titles.iter().position(|s| *s == h2) {
                if idx < last_allowed_idx {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!("section '{}' appears out of order", h2),
                    });
                }
                last_allowed_idx = idx;
            }
        }

        // Check required sections are present
        for section in &structure.sections {
            if section.required && !h2s.iter().any(|h| h == &section.title) {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("missing required section '{}'", section.title),
                });
            }
        }

        // Validate section content format (bullets by default)
        validate_section_content(doc, structure, errors);
    }
}

/// Validate intro content (between H1 and first H2).
fn validate_intro_content(
    doc: &Document,
    intro_def: &super::schema::SectionDef,
    errors: &mut Vec<ValidationError>,
) {
    // Find H1 position
    let h1_pos = doc
        .blocks
        .iter()
        .position(|b| matches!(b, Block::Heading { level: 1, .. }));

    // Find first H2 position
    let first_h2_pos = doc
        .blocks
        .iter()
        .position(|b| matches!(b, Block::Heading { level: 2, .. }));

    let Some(start) = h1_pos else {
        return; // No H1, already reported elsewhere
    };

    let end = first_h2_pos.unwrap_or(doc.blocks.len());

    // Check intro content (blocks between H1 and first H2)
    if intro_def.paragraph {
        return; // Paragraphs allowed, no validation needed
    }

    errors.extend(validate_bullets_only(&doc.blocks[start + 1..end], "intro"));
}

/// Validate that section content matches expected format (bullets by default).
fn validate_section_content(
    doc: &Document,
    structure: &super::schema::StructureDef,
    errors: &mut Vec<ValidationError>,
) {
    use super::schema::{matches_template, parse_template};

    // Find H2 positions in the block list
    let h2_positions: Vec<(usize, String)> = doc
        .blocks
        .iter()
        .enumerate()
        .filter_map(|(i, b)| match b {
            Block::Heading { level: 2, content } => Some((i, super::inlines_to_string(content))),
            _ => None,
        })
        .collect();

    for (pos_idx, (start_pos, section_title)) in h2_positions.iter().enumerate() {
        // Find the section definition
        let Some(section_def) = structure
            .sections
            .iter()
            .find(|s| &s.title == section_title)
        else {
            continue; // Unknown section, already reported
        };

        // Skip if paragraphs are allowed
        if section_def.paragraph {
            continue;
        }

        // Parse template if present
        let template_segments = section_def.template.as_ref().map(|t| parse_template(t));

        // Find end position (next H2 or end of document)
        let end_pos = h2_positions
            .get(pos_idx + 1)
            .map(|(pos, _)| *pos)
            .unwrap_or(doc.blocks.len());

        let section_blocks = &doc.blocks[start_pos + 1..end_pos];

        // Validate bullets only (no paragraphs/code blocks)
        errors.extend(validate_bullets_only(
            section_blocks,
            &format!("section '{}'", section_title),
        ));

        // Validate list items against template if present
        if let Some(ref segments) = template_segments {
            for block in section_blocks {
                if let Block::List { items, .. } = block {
                    for item in items {
                        let item_text = format!("- {}", super::inlines_to_markdown(&item.content));
                        if !matches_template(&item_text, segments) {
                            errors.push(ValidationError {
                                line: 0,
                                message: format!(
                                    "section '{}': item doesn't match template '{}'",
                                    section_title,
                                    section_def.template.as_deref().unwrap_or("")
                                ),
                            });
                        }
                    }
                }
            }
        }
    }
}

/// Validate frontmatter fields against type definition.
fn validate_fields(fm: &super::Frontmatter, type_def: &TypeDef, errors: &mut Vec<ValidationError>) {
    for (field_name, field_def) in &type_def.fields {
        // Check if field exists in frontmatter
        let exists = field_exists(fm, field_name);

        if field_def.required && !exists {
            errors.push(ValidationError {
                line: 0,
                message: format!("missing required field '{}'", field_name),
            });
            continue;
        }

        // Validate field type if value exists in extra map
        // (known fields like 'name' are already type-validated by serde)
        if let Some(val) = get_field_value(fm, field_name) {
            validate_field_type(field_name, val, field_def, errors);
        }
    }
}

/// Check if a field exists in frontmatter.
///
/// Known fields (created, description, name, type) are deserialized into typed fields.
/// Unknown fields go into the `extra` map.
fn field_exists(fm: &super::Frontmatter, field_name: &str) -> bool {
    match field_name {
        "created" => fm.created.is_some(),
        "description" => fm.description.is_some(),
        "name" => fm.name.is_some(),
        "type" => fm.doc_type.is_some(),
        _ => fm.extra.contains_key(field_name),
    }
}

/// Get a field value from frontmatter for type validation.
///
/// Returns the value from the `extra` map for unknown fields.
/// For known fields, returns None (they're validated by their typed presence).
fn get_field_value<'a>(
    fm: &'a super::Frontmatter,
    field_name: &str,
) -> Option<&'a serde_yaml::Value> {
    fm.extra.get(field_name)
}

/// Validate a field's value against its expected type.
fn validate_field_type(
    field_name: &str,
    value: &serde_yaml::Value,
    field_def: &super::schema::FieldDef,
    errors: &mut Vec<ValidationError>,
) {
    match field_def.field_type {
        FieldType::String => {
            if !value.is_string() && !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be a string", field_name),
                });
            }
        }
        FieldType::Date => {
            if let Some(s) = value.as_str() {
                if parse_date(s).is_none() {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!(
                            "field '{}' must be a valid date (got '{}')",
                            field_name, s
                        ),
                    });
                }
            } else if !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be a date string", field_name),
                });
            }
        }
        FieldType::Datetime => {
            if let Some(s) = value.as_str() {
                if parse_datetime(s).is_none() {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!(
                            "field '{}' must be a valid datetime (got '{}')",
                            field_name, s
                        ),
                    });
                }
            } else if !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be a datetime string", field_name),
                });
            }
        }
        FieldType::Integer => {
            if !value.is_i64() && !value.is_u64() && !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be an integer", field_name),
                });
            }
        }
        FieldType::Bool => {
            if !value.is_bool() && !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be a boolean", field_name),
                });
            }
        }
        FieldType::Enum => {
            if let Some(valid_values) = &field_def.values {
                if let Some(s) = value.as_str() {
                    if !valid_values.contains(&s.to_string()) {
                        errors.push(ValidationError {
                            line: 0,
                            message: format!(
                                "field '{}' must be one of: {} (got '{}')",
                                field_name,
                                valid_values.join(", "),
                                s
                            ),
                        });
                    }
                } else if !value.is_null() {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!("field '{}' must be a string enum value", field_name),
                    });
                }
            }
        }
        FieldType::Link => {
            if !value.is_string() && !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be a link string", field_name),
                });
            }
            // Note: Link existence validation requires ctx, handled separately
        }
        FieldType::List => {
            if !value.is_sequence() && !value.is_null() {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("field '{}' must be a list", field_name),
                });
            }
            // Item type validation could be added here
        }
    }
}

/// Validate frontmatter link fields for existence.
fn validate_frontmatter_links(
    fm: &super::Frontmatter,
    type_def: &TypeDef,
    ctx: &FormatContext,
    errors: &mut Vec<ValidationError>,
) {
    for (field_name, field_def) in &type_def.fields {
        if field_def.field_type != FieldType::Link {
            continue;
        }

        let Some(value) = fm.extra.get(field_name) else {
            continue;
        };

        if let Some(link) = value.as_str() {
            if let Some(err) = validate_link_exists(link, ctx) {
                errors.push(err);
            }
        }
    }
}

/// Parse a date string in various formats.
fn parse_date(s: &str) -> Option<NaiveDate> {
    // Try common formats
    let formats = [
        "%Y-%m-%d",  // 2024-03-15
        "%Y/%m/%d",  // 2024/03/15
        "%B %d, %Y", // March 15, 2024
        "%b %d, %Y", // Mar 15, 2024
        "%d %B %Y",  // 15 March 2024
        "%d %b %Y",  // 15 Mar 2024
    ];

    for fmt in formats {
        if let Ok(date) = NaiveDate::parse_from_str(s, fmt) {
            return Some(date);
        }
    }

    // Try parsing ISO 8601 datetime and extract date
    if let Some(dt) = parse_datetime(s) {
        return Some(dt.date());
    }

    None
}

/// Parse a datetime string in various formats.
fn parse_datetime(s: &str) -> Option<chrono::NaiveDateTime> {
    use chrono::{DateTime, NaiveDateTime};

    // Try ISO 8601 with timezone
    if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
        return Some(dt.naive_utc());
    }

    // Try common formats
    let formats = [
        "%Y-%m-%d %H:%M:%S",    // 2024-03-15 08:07:43
        "%Y-%m-%d %H:%M",       // 2024-03-15 08:07
        "%Y-%m-%dT%H:%M:%S",    // 2024-03-15T08:07:43
        "%Y-%m-%dT%H:%M",       // 2024-03-15T08:07
        "%Y-%m-%d %H:%M:%S %z", // 2024-03-15 08:07:43 +0000
        "%Y-%m-%d %H:%M:%S %Z", // 2024-03-15 08:07:43 UTC
    ];

    for fmt in formats {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, fmt) {
            return Some(dt);
        }
    }

    // Try parsing with timezone suffix like "+0000 UTC"
    let s_cleaned = s.trim_end_matches(" UTC").trim_end_matches(" +0000 UTC");
    for fmt in formats {
        if let Ok(dt) = NaiveDateTime::parse_from_str(s_cleaned, fmt) {
            return Some(dt);
        }
    }

    None
}

/// Normalize a document according to its schema type definition.
///
/// This normalizes date/datetime values to their canonical formats:
/// - `date` fields → YYYY-MM-DD
/// - `datetime` fields → ISO 8601 with timezone
///
/// Also removes empty optional sections from the document body.
pub fn normalize(doc: &mut Document, _ctx: &FormatContext, schema: &Schema, type_name: &str) {
    let Some(type_def) = schema.get_type(type_name) else {
        return;
    };

    // Normalize frontmatter fields
    if let Some(ref mut fm) = doc.frontmatter {
        for (field_name, field_def) in &type_def.fields {
            // Check if field is in the extra map
            if let Some(value) = fm.extra.get(field_name) {
                let normalized = match field_def.field_type {
                    FieldType::Date => normalize_date_value(value),
                    FieldType::Datetime => normalize_datetime_value(value),
                    _ => None,
                };

                if let Some(new_value) = normalized {
                    let _ = fm.extra.insert(field_name.clone(), new_value);
                }
            }
        }
    }

    // Prune empty optional sections
    prune_empty_sections(doc, type_def);
}

/// Remove empty optional sections from the document.
///
/// An empty section is a heading followed only by blank lines before the next
/// heading of same or higher level (or EOF). Required sections are preserved
/// even if empty.
fn prune_empty_sections(doc: &mut Document, type_def: &TypeDef) {
    // Build set of required section titles
    let required_titles: std::collections::HashSet<&str> = type_def
        .structure
        .sections
        .iter()
        .filter(|s| s.required)
        .map(|s| s.title.as_str())
        .collect();

    // Find indices of empty optional H2 sections to remove
    let mut to_remove: Vec<usize> = Vec::new();

    let mut i = 0;
    while i < doc.blocks.len() {
        if let Block::Heading { level: 2, content } = &doc.blocks[i] {
            let title = super::inlines_to_string(content);

            // Skip required sections
            if required_titles.contains(title.as_str()) {
                i += 1;
                continue;
            }

            // Check if this section is empty (only blank lines until next heading or EOF)
            let section_start = i;
            let mut j = i + 1;
            let mut is_empty = true;

            while j < doc.blocks.len() {
                match &doc.blocks[j] {
                    Block::BlankLine => {
                        j += 1;
                    }
                    Block::Heading { level, .. } if *level <= 2 => {
                        // Hit next section or higher-level heading
                        break;
                    }
                    _ => {
                        // Found content
                        is_empty = false;
                        break;
                    }
                }
            }

            if is_empty {
                // Mark this heading and its trailing blank lines for removal
                for idx in section_start..j {
                    to_remove.push(idx);
                }
            }
        }
        i += 1;
    }

    // Remove blocks in reverse order to preserve indices
    for idx in to_remove.into_iter().rev() {
        let _ = doc.blocks.remove(idx);
    }
}

/// Normalize a date value to YYYY-MM-DD format.
fn normalize_date_value(value: &serde_yaml::Value) -> Option<serde_yaml::Value> {
    let s = value.as_str()?;
    let date = parse_date(s)?;
    Some(serde_yaml::Value::String(
        date.format("%Y-%m-%d").to_string(),
    ))
}

/// Normalize a datetime value to ISO 8601 format.
fn normalize_datetime_value(value: &serde_yaml::Value) -> Option<serde_yaml::Value> {
    use chrono::{DateTime, FixedOffset, Local, TimeZone};

    let s = value.as_str()?;

    // Try parsing as RFC 3339 first
    if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
        return Some(serde_yaml::Value::String(dt.to_rfc3339()));
    }

    // Try parsing with various formats and convert to RFC 3339
    let naive = parse_datetime(s)?;

    // If we have a naive datetime, assume local timezone
    let local_dt = Local.from_local_datetime(&naive).single()?;
    let fixed: DateTime<FixedOffset> = local_dt.into();
    Some(serde_yaml::Value::String(fixed.to_rfc3339()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::parse;

    /// Build a schema with a single type from a type definition YAML.
    fn make_schema(type_name: &str, yaml: &str) -> Schema {
        let type_def: TypeDef = serde_yaml::from_str(yaml).expect("type should parse");
        let mut schema = Schema::default();
        let _ = schema.types.insert(type_name.to_string(), type_def);
        schema
    }

    #[test]
    fn test_validate_required_fields() {
        let schema = make_schema(
            "person",
            r#"
fields:
  name:
    type: string
    required: true
  age:
    type: integer
    required: false
"#,
        );

        // Missing required field
        let doc = parse(
            r#"---
type: person
age: 30
---
# John
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "person");
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("missing required field 'name'"));
    }

    #[test]
    fn test_validate_valid_document() {
        let schema = make_schema(
            "person",
            r#"
fields:
  name:
    type: string
    required: true
  born:
    type: date
    required: false
"#,
        );

        let doc = parse(
            r#"---
type: person
name: John Doe
born: 2024-03-15
---
# John Doe
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "person");
        assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);
    }

    #[test]
    fn test_validate_enum_field() {
        let schema = make_schema(
            "tvshow",
            r#"
fields:
  status:
    type: enum
    values: [watching, completed, dropped]
    required: true
"#,
        );

        // Valid enum value
        let doc = parse(
            r#"---
type: tvshow
status: watching
---
# Show
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "tvshow");
        assert!(errors.is_empty());

        // Invalid enum value
        let doc = parse(
            r#"---
type: tvshow
status: unknown
---
# Show
"#,
        );

        let errors = validate(&doc, &ctx, &schema, "tvshow");
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("must be one of"));
    }

    #[test]
    fn test_validate_integer_field() {
        let schema = make_schema(
            "test",
            r#"
fields:
  count:
    type: integer
    required: true
"#,
        );

        let doc = parse(
            r#"---
type: test
count: 42
---
# Test
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "test");
        assert!(errors.is_empty());
    }

    #[test]
    fn test_parse_date_formats() {
        assert!(parse_date("2024-03-15").is_some());
        assert!(parse_date("2024/03/15").is_some());
        assert!(parse_date("March 15, 2024").is_some());
        assert!(parse_date("15 March 2024").is_some());
        assert!(parse_date("invalid").is_none());
    }

    #[test]
    fn test_parse_datetime_formats() {
        assert!(parse_datetime("2024-03-15T08:07:43-04:00").is_some());
        assert!(parse_datetime("2024-03-15 08:07:43").is_some());
        assert!(parse_datetime("2024-03-15 08:07").is_some());
        assert!(parse_datetime("invalid").is_none());
    }

    #[test]
    fn test_normalize_date_field() {
        let schema = make_schema(
            "event",
            r#"
fields:
  date:
    type: date
    required: true
"#,
        );

        let mut doc = parse(
            r#"---
type: event
date: March 15, 2024
---
# Event
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        normalize(&mut doc, &ctx, &schema, "event");

        // Check that date was normalized
        let fm = doc.frontmatter.as_ref().expect("should have frontmatter");
        let date_val = fm.extra.get("date").expect("should have date");
        assert_eq!(date_val.as_str(), Some("2024-03-15"));
    }
}

//! Schema-driven markdown validation and normalization.
//!
//! This module handles documents that are validated against `.meow.d/` definitions.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use super::{
    normalize_path,
    schema::{FieldType, LinksDef, Schema, TypeDef},
    urlencoding_decode, validate_bullets_only, validate_link_exists, Block, Document,
    FormatContext, Inline, ValidationError,
};
use chrono::NaiveDate;

// Cache for parsed document frontmatter types.
// Key is absolute path, value is the document's type field.
thread_local! {
    static TYPE_CACHE: RefCell<HashMap<PathBuf, Option<String>>> = RefCell::new(HashMap::new());
}

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
    validate_structure(doc, ctx, schema, type_name, type_def, &mut errors);
    errors
}

/// Validate document structure against type definition.
fn validate_structure(
    doc: &Document,
    ctx: &FormatContext,
    schema: &Schema,
    source_type: &str,
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
        validate_section_content(doc, structure, schema, source_type, ctx, errors);
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
    schema: &Schema,
    source_type: &str,
    ctx: &FormatContext,
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

        // Validate section link constraints
        if let Some(ref links_def) = section_def.links {
            validate_section_links(
                doc,
                section_title,
                links_def,
                schema,
                source_type,
                ctx,
                errors,
            );
        }
    }
}

/// Validate link constraints for a section.
fn validate_section_links(
    doc: &Document,
    section_title: &str,
    links_def: &LinksDef,
    schema: &Schema,
    source_type: &str,
    ctx: &FormatContext,
    errors: &mut Vec<ValidationError>,
) {
    // Extract links from this section
    let links = extract_section_links(doc, section_title);

    if links.is_empty() {
        return;
    }

    // Validate each link
    for link_url in &links {
        // Skip external URLs
        if link_url.starts_with("http://") || link_url.starts_with("https://") {
            continue;
        }

        // Resolve link to absolute path
        let Some(target_path) = resolve_link_path(link_url, ctx) else {
            continue; // Path resolution failed, already reported by validate_link_exists
        };

        // Get target's type
        let target_type = get_cached_type(&target_path);

        // Validate target_type constraint
        if let Some(ref expected_type) = links_def.target_type {
            match &target_type {
                Some(actual_type) if actual_type != expected_type => {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!(
                            "section '{}': link to '{}' must be type '{}', got '{}'",
                            section_title, link_url, expected_type, actual_type
                        ),
                    });
                    continue;
                }
                None => {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!(
                            "section '{}': link target '{}' has no type field",
                            section_title, link_url
                        ),
                    });
                    continue;
                }
                _ => {} // Type matches
            }
        }

        // Validate bidirectional link
        if links_def.bidirectional {
            if let Some(ref target_type_name) = target_type {
                let result = validate_bidirectional_link(
                    &target_path,
                    target_type_name,
                    source_type,
                    ctx.path,
                    schema,
                );

                match result {
                    BidirectionalResult::Ok => {}
                    BidirectionalResult::TargetTypeNotInSchema => {
                        errors.push(ValidationError {
                            line: 0,
                            message: format!(
                                "section '{}': bidirectional link to '{}' - target type '{}' not in schema",
                                section_title, link_url, target_type_name
                            ),
                        });
                    }
                    BidirectionalResult::NoInverseSection => {
                        errors.push(ValidationError {
                            line: 0,
                            message: format!(
                                "section '{}': bidirectional link to '{}' - target type '{}' has no section linking to '{}'",
                                section_title, link_url, target_type_name, source_type
                            ),
                        });
                    }
                    BidirectionalResult::MissingBacklink { inverse_section } => {
                        let source_filename = ctx
                            .path
                            .file_name()
                            .map(|s| s.to_string_lossy())
                            .unwrap_or_default();
                        let target_filename = target_path
                            .file_name()
                            .map(|s| s.to_string_lossy())
                            .unwrap_or_default();

                        errors.push(ValidationError {
                            line: 0,
                            message: format!(
                                "section '{}': bidirectional link - '{}' links to '{}' but '{}' section '{}' doesn't link back",
                                section_title,
                                source_filename,
                                target_filename,
                                target_filename,
                                inverse_section
                            ),
                        });
                    }
                }
            }
        }
    }
}

/// Extract all link URLs from a section's bullet points.
fn extract_section_links(doc: &Document, section_title: &str) -> Vec<String> {
    let mut links = Vec::new();

    // Find the section's H2 position
    let h2_positions: Vec<(usize, String)> = doc
        .blocks
        .iter()
        .enumerate()
        .filter_map(|(i, b)| match b {
            Block::Heading { level: 2, content } => Some((i, super::inlines_to_string(content))),
            _ => None,
        })
        .collect();

    // Find our section
    let section_idx = h2_positions
        .iter()
        .position(|(_, title)| title == section_title);

    let Some(idx) = section_idx else {
        return links;
    };

    let start_pos = h2_positions[idx].0;
    let end_pos = h2_positions
        .get(idx + 1)
        .map(|(pos, _)| *pos)
        .unwrap_or(doc.blocks.len());

    // Extract links from blocks in this section
    for block in &doc.blocks[start_pos + 1..end_pos] {
        extract_links_from_block(block, &mut links);
    }

    links
}

/// Recursively extract link URLs from a block.
fn extract_links_from_block(block: &Block, links: &mut Vec<String>) {
    match block {
        Block::List { items, .. } => {
            for item in items {
                extract_links_from_inlines(&item.content, links);
                for child in &item.children {
                    extract_links_from_block(child, links);
                }
            }
        }
        Block::Paragraph(content) => {
            extract_links_from_inlines(content, links);
        }
        _ => {}
    }
}

/// Extract link URLs from inline elements.
fn extract_links_from_inlines(inlines: &[Inline], links: &mut Vec<String>) {
    for inline in inlines {
        match inline {
            Inline::Link { url, .. } => {
                links.push(url.clone());
            }
            Inline::Strong(inner) | Inline::Emphasis(inner) => {
                extract_links_from_inlines(inner, links);
            }
            _ => {}
        }
    }
}

/// Resolve a relative link to an absolute path.
fn resolve_link_path(link: &str, ctx: &FormatContext) -> Option<PathBuf> {
    // Skip external URLs
    if link.starts_with("http://") || link.starts_with("https://") {
        return None;
    }

    // Skip anchor-only links
    if link.starts_with('#') {
        return None;
    }

    // URL-decode the path
    let decoded = urlencoding_decode(link);
    let link_path = Path::new(&decoded);

    // Resolve relative path against the file's directory
    let base_dir = ctx.path.parent()?;
    let resolved = base_dir.join(link_path);

    // Normalize the path (resolve .. and .)
    Some(normalize_path(&resolved))
}

/// Get a file's frontmatter type, using cache.
fn get_cached_type(path: &Path) -> Option<String> {
    TYPE_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();

        if let Some(cached) = cache.get(path) {
            return cached.clone();
        }

        // Read and parse the file
        let doc_type = read_frontmatter_type(path);
        let _ = cache.insert(path.to_path_buf(), doc_type.clone());
        doc_type
    })
}

/// Read just the frontmatter type field from a file.
fn read_frontmatter_type(path: &Path) -> Option<String> {
    let content = std::fs::read_to_string(path).ok()?;
    let doc = super::parse(&content);
    doc.frontmatter?.doc_type
}

/// Result of bidirectional link validation.
enum BidirectionalResult {
    /// Link is valid (backlink exists).
    Ok,
    /// Target type not found in schema.
    TargetTypeNotInSchema,
    /// Target type has no section linking back to source type.
    NoInverseSection,
    /// Backlink is missing.
    MissingBacklink { inverse_section: String },
}

/// Validate that a bidirectional link exists from target back to source.
#[allow(clippy::too_many_arguments)]
fn validate_bidirectional_link(
    target_path: &Path,
    target_type: &str,
    source_type: &str,
    source_path: &Path,
    schema: &Schema,
) -> BidirectionalResult {
    // Find the inverse section in the target type's schema
    let Some(target_type_def) = schema.get_type(target_type) else {
        return BidirectionalResult::TargetTypeNotInSchema;
    };

    // Find a section in target schema that links back to source_type
    let inverse_section = target_type_def.structure.sections.iter().find(|s| {
        s.links
            .as_ref()
            .and_then(|l| l.target_type.as_ref())
            .map(|t| t == source_type)
            .unwrap_or(false)
    });

    let Some(inverse_section) = inverse_section else {
        return BidirectionalResult::NoInverseSection;
    };

    // Parse target document and check for backlink
    let Ok(target_content) = std::fs::read_to_string(target_path) else {
        return BidirectionalResult::Ok; // Can't read target, assume ok
    };

    let target_doc = super::parse(&target_content);
    let target_links = extract_section_links(&target_doc, &inverse_section.title);

    // Check if any link in target points back to source
    let has_backlink = target_links.iter().any(|target_link| {
        // Resolve the target's link relative to target file
        let target_dir = target_path.parent();
        if let Some(target_dir) = target_dir {
            let decoded = urlencoding_decode(target_link);
            let resolved = normalize_path(&target_dir.join(Path::new(&decoded)));
            resolved == source_path
        } else {
            false
        }
    });

    if has_backlink {
        BidirectionalResult::Ok
    } else {
        BidirectionalResult::MissingBacklink {
            inverse_section: inverse_section.title.clone(),
        }
    }
}

/// Validate frontmatter fields against type definition.
fn validate_fields(fm: &super::Frontmatter, type_def: &TypeDef, errors: &mut Vec<ValidationError>) {
    // Validate structure.frontmatter required fields
    for field_def in &type_def.structure.frontmatter {
        if field_def.required && !field_exists(fm, &field_def.name) {
            errors.push(ValidationError {
                line: 0,
                message: format!("missing required field '{}'", field_def.name),
            });
        }
    }

    // Validate typed fields
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

    #[test]
    fn test_validate_section_link_target_type() {
        use std::io::Write;

        // Create temp directory with test files
        let dir = tempfile::tempdir().expect("create tempdir");
        let threats_dir = dir.path().join("threats");
        let mitigations_dir = dir.path().join("mitigations");
        std::fs::create_dir_all(&threats_dir).expect("create threats dir");
        std::fs::create_dir_all(&mitigations_dir).expect("create mitigations dir");

        // Create a mitigation file
        let mitigation_path = mitigations_dir.join("Firewall.md");
        let mut f = std::fs::File::create(&mitigation_path).expect("create mitigation");
        writeln!(f, "---\ntype: mitigation\n---\n# Firewall").expect("write mitigation");

        // Create a wrong-type file (a threat, not mitigation)
        let wrong_type_path = mitigations_dir.join("Wrong.md");
        let mut f = std::fs::File::create(&wrong_type_path).expect("create wrong type");
        writeln!(f, "---\ntype: threat\n---\n# Wrong").expect("write wrong type");

        // Build schema with both types
        let mut schema = Schema::default();
        let threat_def: TypeDef = serde_yaml::from_str(
            r#"
structure:
  sections:
    - title: Mitigated By
      links:
        target_type: mitigation
"#,
        )
        .expect("parse threat type");
        let mitigation_def: TypeDef = serde_yaml::from_str(
            r#"
structure:
  sections:
    - title: Counters
"#,
        )
        .expect("parse mitigation type");
        let _ = schema.types.insert("threat".to_string(), threat_def);
        let _ = schema
            .types
            .insert("mitigation".to_string(), mitigation_def);

        // Test: link to correct type passes
        let threat_path = threats_dir.join("SQL Injection.md");
        let doc = parse(
            r#"---
type: threat
---
# SQL Injection

## Mitigated By

- [Firewall](../mitigations/Firewall.md)
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: &threat_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "threat");
        assert!(
            errors.is_empty(),
            "expected no errors for correct type, got: {:?}",
            errors
        );

        // Test: link to wrong type errors
        let doc = parse(
            r#"---
type: threat
---
# SQL Injection

## Mitigated By

- [Wrong](../mitigations/Wrong.md)
"#,
        );

        let errors = validate(&doc, &ctx, &schema, "threat");
        assert_eq!(errors.len(), 1, "expected 1 error, got: {:?}", errors);
        assert!(
            errors[0].message.contains("must be type 'mitigation'"),
            "expected type mismatch error, got: {}",
            errors[0].message
        );
    }

    #[test]
    fn test_validate_section_link_bidirectional() {
        use std::io::Write;

        // Create temp directory with test files
        let dir = tempfile::tempdir().expect("create tempdir");
        let threats_dir = dir.path().join("threats");
        let mitigations_dir = dir.path().join("mitigations");
        std::fs::create_dir_all(&threats_dir).expect("create threats dir");
        std::fs::create_dir_all(&mitigations_dir).expect("create mitigations dir");

        // Create threat file
        let threat_path = threats_dir.join("SQL Injection.md");
        let mut f = std::fs::File::create(&threat_path).expect("create threat");
        writeln!(
            f,
            r#"---
type: threat
---
# SQL Injection

## Mitigated By

- [Firewall](../mitigations/Firewall.md)
"#
        )
        .expect("write threat");

        // Create mitigation WITH backlink
        let mitigation_with_backlink = mitigations_dir.join("Firewall.md");
        let mut f = std::fs::File::create(&mitigation_with_backlink).expect("create mitigation");
        writeln!(
            f,
            r#"---
type: mitigation
---
# Firewall

## Counters

- [SQL Injection](../threats/SQL%20Injection.md)
"#
        )
        .expect("write mitigation");

        // Create mitigation WITHOUT backlink
        let mitigation_no_backlink = mitigations_dir.join("NoBacklink.md");
        let mut f =
            std::fs::File::create(&mitigation_no_backlink).expect("create mitigation no backlink");
        writeln!(
            f,
            r#"---
type: mitigation
---
# NoBacklink

## Counters

- [Other Threat](../threats/Other.md)
"#
        )
        .expect("write mitigation no backlink");

        // Build schema with bidirectional links
        let mut schema = Schema::default();
        let threat_def: TypeDef = serde_yaml::from_str(
            r#"
structure:
  sections:
    - title: Mitigated By
      links:
        target_type: mitigation
        bidirectional: true
"#,
        )
        .expect("parse threat type");
        let mitigation_def: TypeDef = serde_yaml::from_str(
            r#"
structure:
  sections:
    - title: Counters
      links:
        target_type: threat
"#,
        )
        .expect("parse mitigation type");
        let _ = schema.types.insert("threat".to_string(), threat_def);
        let _ = schema
            .types
            .insert("mitigation".to_string(), mitigation_def);

        // Test: bidirectional link with backlink passes
        let doc = parse(
            r#"---
type: threat
---
# SQL Injection

## Mitigated By

- [Firewall](../mitigations/Firewall.md)
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: &threat_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "threat");
        assert!(
            errors.is_empty(),
            "expected no errors when backlink exists, got: {:?}",
            errors
        );

        // Test: bidirectional link without backlink errors
        let doc = parse(
            r#"---
type: threat
---
# SQL Injection

## Mitigated By

- [NoBacklink](../mitigations/NoBacklink.md)
"#,
        );

        let errors = validate(&doc, &ctx, &schema, "threat");
        assert_eq!(
            errors.len(),
            1,
            "expected 1 error for missing backlink, got: {:?}",
            errors
        );
        assert!(
            errors[0].message.contains("doesn't link back"),
            "expected backlink error, got: {}",
            errors[0].message
        );
    }

    #[test]
    fn test_validate_section_link_no_type_field() {
        use std::io::Write;

        // Create temp directory with test files
        let dir = tempfile::tempdir().expect("create tempdir");
        let threats_dir = dir.path().join("threats");
        let mitigations_dir = dir.path().join("mitigations");
        std::fs::create_dir_all(&threats_dir).expect("create threats dir");
        std::fs::create_dir_all(&mitigations_dir).expect("create mitigations dir");

        // Create a file with no type field
        let no_type_path = mitigations_dir.join("NoType.md");
        let mut f = std::fs::File::create(&no_type_path).expect("create no type file");
        writeln!(f, "---\nname: NoType\n---\n# NoType").expect("write no type");

        // Build schema
        let mut schema = Schema::default();
        let threat_def: TypeDef = serde_yaml::from_str(
            r#"
structure:
  sections:
    - title: Mitigated By
      links:
        target_type: mitigation
"#,
        )
        .expect("parse threat type");
        let _ = schema.types.insert("threat".to_string(), threat_def);

        let threat_path = threats_dir.join("Test.md");
        let doc = parse(
            r#"---
type: threat
---
# Test

## Mitigated By

- [NoType](../mitigations/NoType.md)
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: &threat_path,
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "threat");
        assert_eq!(errors.len(), 1, "expected 1 error, got: {:?}", errors);
        assert!(
            errors[0].message.contains("has no type field"),
            "expected no type field error, got: {}",
            errors[0].message
        );
    }

    #[test]
    fn test_validate_structure_frontmatter_required() {
        let schema = make_schema(
            "target",
            r#"
structure:
  frontmatter:
    - name: category
      required: true
    - name: priority
      required: false
"#,
        );

        // Missing required field
        let doc = parse(
            r#"---
type: target
priority: high
---
# Test Target
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "target");
        assert_eq!(errors.len(), 1, "expected 1 error, got: {:?}", errors);
        assert!(
            errors[0]
                .message
                .contains("missing required field 'category'"),
            "expected missing category error, got: {}",
            errors[0].message
        );
    }

    #[test]
    fn test_validate_structure_frontmatter_valid() {
        let schema = make_schema(
            "target",
            r#"
structure:
  frontmatter:
    - name: category
      required: true
    - name: priority
      required: false
"#,
        );

        // Has required field
        let doc = parse(
            r#"---
type: target
category: account
---
# Test Target
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "target");
        assert!(errors.is_empty(), "expected no errors, got: {:?}", errors);
    }

    #[test]
    fn test_validate_structure_frontmatter_multiple_required() {
        let schema = make_schema(
            "threat",
            r#"
structure:
  frontmatter:
    - name: likelihood
      required: true
    - name: impact
      required: true
"#,
        );

        // Missing both required fields
        let doc = parse(
            r#"---
type: threat
---
# Test Threat
"#,
        );

        let ctx = FormatContext {
            project: "test",
            path: std::path::Path::new("test.md"),
            year_month: None,
            git_tree: None,
            repo_root: None,
        };

        let errors = validate(&doc, &ctx, &schema, "threat");
        assert_eq!(errors.len(), 2, "expected 2 errors, got: {:?}", errors);
        assert!(errors.iter().any(|e| e.message.contains("'likelihood'")));
        assert!(errors.iter().any(|e| e.message.contains("'impact'")));
    }
}

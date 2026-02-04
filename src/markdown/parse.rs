//! Markdown parsing and serialization using pulldown-cmark.

use super::{Block, Document, Frontmatter, Inline, ListItem};
use crate::crypto;
use anyhow::{Context, Result};
use gray_matter::{engine::YAML, Matter};
use pulldown_cmark::{CodeBlockKind, Event, HeadingLevel, Parser, Tag, TagEnd};
use std::collections::VecDeque;

/// Parse markdown content into a Document AST.
pub fn parse(content: &str) -> Document {
    // Extract frontmatter first
    let matter = Matter::<YAML>::new();
    let parsed = matter.parse(content);

    let frontmatter: Option<Frontmatter> = parsed.data.and_then(|d| d.deserialize().ok());

    // Parse the markdown body - collect events first to avoid borrow issues
    let parser = Parser::new(&parsed.content);
    let events: VecDeque<Event> = parser.collect();
    let blocks = parse_blocks(events);

    // Compute 1-based line numbers for each block
    let fm_lines = count_frontmatter_lines(content);
    let block_lines = compute_block_lines(&parsed.content, &blocks, fm_lines);

    Document {
        frontmatter,
        blocks,
        block_lines,
    }
}

/// Parse an encrypted markdown file.
///
/// Extracts frontmatter (always plaintext), then decrypts the body if encrypted.
/// Returns the decrypted document and the original raw body (for change detection).
pub fn parse_encrypted(content: &str) -> Result<(Document, Option<String>)> {
    let matter = Matter::<YAML>::new();
    let parsed = matter.parse(content);

    let frontmatter: Option<Frontmatter> = parsed.data.and_then(|d| d.deserialize().ok());

    // Check if encrypted
    let is_encrypted = frontmatter.as_ref().is_some_and(|fm| fm.encrypted);

    if !is_encrypted {
        // Not encrypted, parse normally
        let parser = Parser::new(&parsed.content);
        let events: VecDeque<Event> = parser.collect();
        let blocks = parse_blocks(events);
        let fm_lines = count_frontmatter_lines(content);
        let block_lines = compute_block_lines(&parsed.content, &blocks, fm_lines);
        return Ok((
            Document {
                frontmatter,
                blocks,
                block_lines,
            },
            None,
        ));
    }

    // Decrypt the body
    let Some(fm) = frontmatter.as_ref() else {
        anyhow::bail!("encrypted file missing frontmatter");
    };
    let wrapped_key = fm
        .wrapped_key
        .as_ref()
        .context("encrypted file missing wrapped-key")?;

    // Unwrap the AES key using GPG
    let aes_key = crypto::unwrap_key_gpg(wrapped_key).context("failed to unwrap encryption key")?;

    // Decode and decrypt the body
    let body_trimmed = parsed.content.trim();
    let encrypted_blob = crypto::decode_base64(body_trimmed).context("failed to decode body")?;
    let decrypted =
        crypto::decrypt_aes_gcm(&encrypted_blob, &aes_key).context("failed to decrypt body")?;

    // Parse decrypted content as markdown
    let decrypted_str =
        String::from_utf8(decrypted).context("decrypted content is not valid UTF-8")?;
    let parser = Parser::new(&decrypted_str);
    let events: VecDeque<Event> = parser.collect();
    let blocks = parse_blocks(events);

    // Return document and original body for change detection
    // Encrypted docs get zeroed line numbers (meaningless for encrypted content)
    let block_lines = vec![0; blocks.len()];
    Ok((
        Document {
            frontmatter,
            blocks,
            block_lines,
        },
        Some(body_trimmed.to_string()),
    ))
}

/// Serialize a document, encrypting the body if the file should be encrypted.
///
/// - `key_id`: GPG key ID for encryption
/// - `existing_wrapped_key`: Reuse existing wrapped key if available (for re-encryption)
/// - If body content matches `original_body_b64`, returns None (no change needed)
pub fn serialize_encrypted(
    doc: &Document,
    field_order: Option<&[&str]>,
    key_id: &str,
    existing_wrapped_key: Option<&str>,
    original_body_b64: Option<&str>,
) -> Result<Option<String>> {
    // First serialize to get the plaintext body
    let plain_output = serialize_with_field_order(doc, field_order);

    // Split into frontmatter and body
    let matter = Matter::<YAML>::new();
    let parsed = matter.parse(&plain_output);
    let body = parsed.content.trim();

    // Check if body changed (comparing plaintext to decrypted original)
    // Note: We can't compare ciphertext directly due to random nonce
    // This optimization assumes caller decrypted original and passes its body
    if let Some(orig) = original_body_b64 {
        // Decrypt original to compare
        if let Some(fm) = &doc.frontmatter {
            if let Some(wrapped) = &fm.wrapped_key {
                if let Ok(key) = crypto::unwrap_key_gpg(wrapped) {
                    if let Ok(blob) = crypto::decode_base64(orig) {
                        if let Ok(decrypted) = crypto::decrypt_aes_gcm(&blob, &key) {
                            if let Ok(orig_body) = String::from_utf8(decrypted) {
                                if orig_body.trim() == body {
                                    return Ok(None); // No change
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Get or generate the AES key
    let (aes_key, wrapped_key) = if let Some(existing) = existing_wrapped_key {
        // Unwrap existing key for re-encryption
        let key = crypto::unwrap_key_gpg(existing).context("failed to unwrap existing key")?;
        (key, existing.to_string())
    } else {
        // Generate new key for first-time encryption
        let key = crypto::generate_key();
        let wrapped = crypto::wrap_key_gpg(&key, key_id).context("failed to wrap new key")?;
        (key.to_vec(), wrapped)
    };

    // Encrypt body
    let encrypted = crypto::encrypt_aes_gcm(body.as_bytes(), &aes_key)?;
    let encrypted_b64 = crypto::encode_base64(&encrypted);

    // Build output with encrypted frontmatter fields
    let mut output = String::new();
    output.push_str("---\n");

    // Serialize existing frontmatter fields (if any)
    if let Some(fm) = &doc.frontmatter {
        // First output type if present
        if let Some(doc_type) = &fm.doc_type {
            output.push_str(&format!("type: {doc_type}\n"));
        }

        // Then known fields
        if let Some(created) = &fm.created {
            output.push_str(&format!("created: {}\n", created.format("%Y-%m-%d")));
        }
        if let Some(description) = &fm.description {
            output.push_str(&format!("description: {description}\n"));
        }
        if let Some(name) = &fm.name {
            output.push_str(&format!("name: {name}\n"));
        }
    }

    // Encryption fields (always added, even if no original frontmatter)
    output.push_str("encrypted: true\n");
    output.push_str(&format!("key-id: {key_id}\n"));
    output.push_str(&format!("wrapped-key: {wrapped_key}\n"));

    // Preserve encrypt config (for README files that configure project encryption)
    if let Some(fm) = &doc.frontmatter {
        if let Some(ref encrypt) = fm.encrypt {
            output.push_str("encrypt:\n");
            if let Some(ref kid) = encrypt.key_id {
                output.push_str(&format!("  key-id: \"{kid}\"\n"));
            }
            if !encrypt.files.is_empty() {
                output.push_str("  files: [");
                output.push_str(&encrypt.files.join(", "));
                output.push_str("]\n");
            }
        }

        // Extra fields (excluding encryption-related ones)
        let mut extra_keys: Vec<_> = fm
            .extra
            .keys()
            .filter(|k| !["encrypted", "key-id", "wrapped-key"].contains(&k.as_str()))
            .collect();
        extra_keys.sort();
        for key in extra_keys {
            if let Some(value) = fm.extra.get(key) {
                serialize_yaml_field(&mut output, key, value, 0);
            }
        }
    }

    output.push_str("---\n");
    output.push_str(&encrypted_b64);
    output.push('\n');

    Ok(Some(output))
}

/// Check if raw file content is encrypted (by checking frontmatter).
pub fn is_encrypted(content: &str) -> bool {
    let matter = Matter::<YAML>::new();
    let parsed = matter.parse(content);
    let frontmatter: Option<Frontmatter> = parsed.data.and_then(|d| d.deserialize().ok());
    frontmatter.is_some_and(|fm| fm.encrypted)
}

/// Get the frontmatter parse error from raw content, if any.
///
/// Returns `Some(error_message)` if frontmatter exists but failed to deserialize,
/// `None` if frontmatter parsed successfully or doesn't exist.
pub fn get_frontmatter_error(content: &str) -> Option<String> {
    let matter = Matter::<YAML>::new();
    let parsed = matter.parse(content);
    parsed
        .data
        .and_then(|d| d.deserialize::<Frontmatter>().err().map(|e| e.to_string()))
}

/// Parse pulldown-cmark events into blocks.
fn parse_blocks(mut events: VecDeque<Event>) -> Vec<Block> {
    let mut blocks = Vec::new();

    while let Some(event) = events.pop_front() {
        match event {
            Event::Start(Tag::Heading { level, .. }) => {
                let content = collect_inlines(&mut events, TagEnd::Heading(level));
                blocks.push(Block::Heading {
                    level: heading_level_to_u8(level),
                    content,
                });
            }
            Event::Start(Tag::Paragraph) => {
                let content = collect_inlines(&mut events, TagEnd::Paragraph);
                if content.is_empty() {
                    blocks.push(Block::BlankLine);
                } else {
                    blocks.push(Block::Paragraph(content));
                }
            }
            Event::Start(Tag::List(start_num)) => {
                let ordered = start_num.is_some();
                let items = collect_list_items(&mut events);
                blocks.push(Block::List { items, ordered });
            }
            Event::Start(Tag::CodeBlock(kind)) => {
                let language = match kind {
                    CodeBlockKind::Fenced(lang) if !lang.is_empty() => Some(lang.to_string()),
                    _ => None,
                };
                let content = collect_code_block(&mut events);
                blocks.push(Block::CodeBlock { language, content });
            }
            _ => {}
        }
    }

    // Normalize: ensure blank lines after headings
    normalize_blank_lines(&mut blocks);

    blocks
}

/// Collect inline events until we hit the end tag.
fn collect_inlines(events: &mut VecDeque<Event>, end_tag: TagEnd) -> Vec<Inline> {
    let mut inlines = Vec::new();

    while let Some(event) = events.pop_front() {
        match event {
            Event::End(tag) if tag == end_tag => break,
            Event::Text(text) => {
                inlines.push(Inline::Text(text.to_string()));
            }
            Event::Code(code) => {
                inlines.push(Inline::Code(code.to_string()));
            }
            Event::Start(Tag::Strong) => {
                let inner = collect_inlines(events, TagEnd::Strong);
                inlines.push(Inline::Strong(inner));
            }
            Event::Start(Tag::Emphasis) => {
                let inner = collect_inlines(events, TagEnd::Emphasis);
                inlines.push(Inline::Emphasis(inner));
            }
            Event::Start(Tag::Link { dest_url, .. }) => {
                let inner = collect_inlines(events, TagEnd::Link);
                let text = super::inlines_to_string(&inner);
                inlines.push(Inline::Link {
                    text,
                    url: dest_url.to_string(),
                });
            }
            Event::SoftBreak => {
                inlines.push(Inline::SoftBreak);
            }
            _ => {}
        }
    }

    inlines
}

/// Collect list items until we hit the end of the list.
fn collect_list_items(events: &mut VecDeque<Event>) -> Vec<ListItem> {
    let mut items = Vec::new();

    while let Some(event) = events.pop_front() {
        match event {
            Event::End(TagEnd::List(_)) => break,
            Event::Start(Tag::Item) => {
                let (content, children) = collect_item_content(events);
                items.push(ListItem { content, children });
            }
            _ => {}
        }
    }

    items
}

/// Collect content for a list item (inline content + nested blocks).
fn collect_item_content(events: &mut VecDeque<Event>) -> (Vec<Inline>, Vec<Block>) {
    let mut inlines = Vec::new();
    let mut children = Vec::new();
    let mut first_paragraph = true;

    while let Some(event) = events.pop_front() {
        match event {
            Event::End(TagEnd::Item) => break,
            Event::Start(Tag::Paragraph) => {
                let para_inlines = collect_inlines(events, TagEnd::Paragraph);
                if first_paragraph {
                    // First paragraph becomes the item's main content
                    inlines = para_inlines;
                    first_paragraph = false;
                } else {
                    // Subsequent paragraphs are nested blocks
                    children.push(Block::Paragraph(para_inlines));
                }
            }
            Event::Start(Tag::List(start_num)) => {
                // Nested list
                let ordered = start_num.is_some();
                let items = collect_list_items(events);
                children.push(Block::List { items, ordered });
            }
            Event::Start(Tag::CodeBlock(kind)) => {
                let language = match kind {
                    CodeBlockKind::Fenced(lang) if !lang.is_empty() => Some(lang.to_string()),
                    _ => None,
                };
                let content = collect_code_block(events);
                children.push(Block::CodeBlock { language, content });
            }
            Event::Text(text) => {
                inlines.push(Inline::Text(text.to_string()));
                first_paragraph = false;
            }
            Event::Code(code) => {
                inlines.push(Inline::Code(code.to_string()));
                first_paragraph = false;
            }
            Event::Start(Tag::Strong) => {
                let inner = collect_inlines(events, TagEnd::Strong);
                inlines.push(Inline::Strong(inner));
                first_paragraph = false;
            }
            Event::Start(Tag::Emphasis) => {
                let inner = collect_inlines(events, TagEnd::Emphasis);
                inlines.push(Inline::Emphasis(inner));
                first_paragraph = false;
            }
            Event::Start(Tag::Link { dest_url, .. }) => {
                let inner = collect_inlines(events, TagEnd::Link);
                let text = super::inlines_to_string(&inner);
                inlines.push(Inline::Link {
                    text,
                    url: dest_url.to_string(),
                });
                first_paragraph = false;
            }
            Event::SoftBreak => {
                inlines.push(Inline::SoftBreak);
            }
            _ => {}
        }
    }

    (inlines, children)
}

/// Collect code block content.
fn collect_code_block(events: &mut VecDeque<Event>) -> String {
    let mut content = String::new();

    while let Some(event) = events.pop_front() {
        match event {
            Event::End(TagEnd::CodeBlock) => break,
            Event::Text(text) => content.push_str(&text),
            _ => {}
        }
    }

    content
}

/// Ensure there's a blank line after each heading.
fn normalize_blank_lines(blocks: &mut Vec<Block>) {
    let mut i = 0;
    while i < blocks.len() {
        if matches!(blocks[i], Block::Heading { .. }) {
            let next_is_blank = blocks
                .get(i + 1)
                .is_some_and(|b| matches!(b, Block::BlankLine));

            if !next_is_blank && i + 1 < blocks.len() {
                blocks.insert(i + 1, Block::BlankLine);
            }
        }
        i += 1;
    }
}

/// Count the number of lines occupied by YAML frontmatter (including `---` delimiters).
/// Returns 0 if there is no frontmatter.
fn count_frontmatter_lines(content: &str) -> usize {
    if !content.starts_with("---") {
        return 0;
    }
    // Find the closing `---` after the opening one
    if let Some(end) = content[3..].find("\n---") {
        // +1 for opening `---\n`, count lines in between, +1 for closing `---\n`
        let frontmatter_section = &content[..3 + end + 4]; // "---\n...\n---"
        frontmatter_section.lines().count()
    } else {
        0
    }
}

/// Compute 1-based line numbers for each block.
///
/// Uses `pulldown_cmark`'s offset iterator to map block-start byte offsets
/// to line numbers. `Block::BlankLine` entries (synthetic, from `normalize_blank_lines`)
/// are assigned line 0 (unknown).
fn compute_block_lines(body: &str, blocks: &[Block], fm_lines: usize) -> Vec<usize> {
    use pulldown_cmark::{Event, Options, Tag};

    // Build a line-start offset table for the body
    let line_starts: Vec<usize> = std::iter::once(0)
        .chain(body.match_indices('\n').map(|(i, _)| i + 1))
        .collect();

    // Collect byte offsets of top-level block-start events
    let parser = pulldown_cmark::Parser::new_ext(body, Options::empty());
    let mut block_offsets: Vec<usize> = Vec::new();
    let mut depth: usize = 0;

    for (event, range) in parser.into_offset_iter() {
        match &event {
            Event::Start(Tag::Heading { .. })
            | Event::Start(Tag::Paragraph)
            | Event::Start(Tag::List(_))
            | Event::Start(Tag::CodeBlock(_)) => {
                if depth == 0 {
                    block_offsets.push(range.start);
                }
                depth += 1;
            }
            Event::End(_) => {
                depth = depth.saturating_sub(1);
            }
            _ => {}
        }
    }

    // Map each block to a line number
    let mut offset_idx = 0;
    let mut result = Vec::with_capacity(blocks.len());

    for block in blocks {
        if matches!(block, Block::BlankLine) {
            result.push(0);
        } else if offset_idx < block_offsets.len() {
            let byte_offset = block_offsets[offset_idx];
            // Binary search for the line containing this offset
            let line_idx = line_starts.partition_point(|&start| start <= byte_offset);
            // line_idx is 1-based within body; add fm_lines for absolute line number
            result.push(line_idx + fm_lines);
            offset_idx += 1;
        } else {
            result.push(0);
        }
    }

    result
}

fn heading_level_to_u8(level: HeadingLevel) -> u8 {
    match level {
        HeadingLevel::H1 => 1,
        HeadingLevel::H2 => 2,
        HeadingLevel::H3 => 3,
        HeadingLevel::H4 => 4,
        HeadingLevel::H5 => 5,
        HeadingLevel::H6 => 6,
    }
}

/// Serialize a Document AST back to markdown string.
pub fn serialize(doc: &Document) -> String {
    serialize_with_field_order(doc, None)
}

/// Serialize a Document AST with optional field ordering.
///
/// If `field_order` is provided, frontmatter fields are output in that order.
/// Otherwise, known fields are output first, then extra fields alphabetically.
pub fn serialize_with_field_order(doc: &Document, field_order: Option<&[&str]>) -> String {
    let mut output = String::new();

    // Write frontmatter
    if let Some(fm) = &doc.frontmatter {
        output.push_str("---\n");

        if let Some(order) = field_order {
            // Schema-ordered serialization
            serialize_frontmatter_ordered(&mut output, fm, order);
        } else {
            // Default serialization: known fields first, then extras alphabetically
            if let Some(created) = &fm.created {
                output.push_str(&format!("created: {}\n", created.format("%Y-%m-%d")));
            }
            if let Some(description) = &fm.description {
                output.push_str(&format!("description: {description}\n"));
            }
            if let Some(name) = &fm.name {
                output.push_str(&format!("name: {name}\n"));
            }
            if let Some(doc_type) = &fm.doc_type {
                output.push_str(&format!("type: {doc_type}\n"));
            }
            // Preserve encrypt config (for README files)
            if let Some(ref encrypt) = fm.encrypt {
                output.push_str("encrypt:\n");
                if let Some(ref kid) = encrypt.key_id {
                    output.push_str(&format!("  key-id: \"{kid}\"\n"));
                }
                if !encrypt.files.is_empty() {
                    output.push_str("  files: [");
                    output.push_str(&encrypt.files.join(", "));
                    output.push_str("]\n");
                }
            }
            // Output extra fields alphabetically
            let mut extra_keys: Vec<_> = fm.extra.keys().collect();
            extra_keys.sort();
            for key in extra_keys {
                if let Some(value) = fm.extra.get(key) {
                    serialize_yaml_field(&mut output, key, value, 0);
                }
            }
        }
        output.push_str("---\n");
    }

    // Write blocks
    for (i, block) in doc.blocks.iter().enumerate() {
        // Add blank line before headings (except first block)
        if matches!(block, Block::Heading { .. })
            && i > 0
            && !matches!(doc.blocks.get(i - 1), Some(Block::BlankLine))
        {
            output.push('\n');
        }

        match block {
            Block::Heading { level, content } => {
                let hashes = "#".repeat(*level as usize);
                output.push_str(&format!("{} {}\n", hashes, serialize_inlines(content)));
            }
            Block::Paragraph(content) => {
                output.push_str(&serialize_inlines(content));
                output.push('\n');
            }
            Block::List { items, ordered } => {
                serialize_list(&mut output, items, *ordered, "");
            }
            Block::CodeBlock { language, content } => {
                let lang = language.as_deref().unwrap_or("");
                output.push_str(&format!("```{lang}\n"));
                output.push_str(content);
                if !content.ends_with('\n') {
                    output.push('\n');
                }
                output.push_str("```\n");
            }
            Block::BlankLine => {
                output.push('\n');
            }
        }
    }

    // Ensure single trailing newline
    while output.ends_with("\n\n\n") {
        let _ = output.pop();
    }
    if !output.ends_with('\n') {
        output.push('\n');
    }

    output
}

/// Serialize inline elements to string.
fn serialize_inlines(inlines: &[Inline]) -> String {
    let mut result = String::new();

    for inline in inlines {
        match inline {
            Inline::Text(s) => result.push_str(s),
            Inline::Strong(inner) => {
                result.push_str("**");
                result.push_str(&serialize_inlines(inner));
                result.push_str("**");
            }
            Inline::Emphasis(inner) => {
                result.push('*');
                result.push_str(&serialize_inlines(inner));
                result.push('*');
            }
            Inline::Link { text, url } => {
                result.push_str(&format!("[{text}]({url})"));
            }
            Inline::Code(s) => {
                result.push_str(&format!("`{s}`"));
            }
            Inline::SoftBreak => {
                result.push('\n');
            }
        }
    }

    result
}

/// Serialize a list with proper indentation for nested content.
fn serialize_list(output: &mut String, items: &[ListItem], ordered: bool, indent: &str) {
    for (idx, item) in items.iter().enumerate() {
        let marker = if ordered {
            format!("{}.", idx + 1)
        } else {
            "-".to_string()
        };

        // Write the marker and main content
        output.push_str(indent);
        output.push_str(&marker);
        output.push(' ');
        output.push_str(&serialize_inlines(&item.content));
        output.push('\n');

        // Write nested children with increased indentation
        if !item.children.is_empty() {
            let child_indent = format!("{}   ", indent); // 3 spaces for nested content
            serialize_blocks(output, &item.children, &child_indent);
        }
    }
}

/// Serialize blocks with indentation prefix.
fn serialize_blocks(output: &mut String, blocks: &[Block], indent: &str) {
    for block in blocks {
        match block {
            Block::Heading { level, content } => {
                let hashes = "#".repeat(*level as usize);
                output.push_str(indent);
                output.push_str(&format!("{} {}\n", hashes, serialize_inlines(content)));
            }
            Block::Paragraph(content) => {
                output.push_str(indent);
                output.push_str(&serialize_inlines(content));
                output.push('\n');
            }
            Block::List { items, ordered } => {
                serialize_list(output, items, *ordered, indent);
            }
            Block::CodeBlock { language, content } => {
                let lang = language.as_deref().unwrap_or("");
                output.push_str(indent);
                output.push_str(&format!("```{lang}\n"));
                for line in content.lines() {
                    output.push_str(indent);
                    output.push_str(line);
                    output.push('\n');
                }
                output.push_str(indent);
                output.push_str("```\n");
            }
            Block::BlankLine => {
                output.push('\n');
            }
        }
    }
}

/// Serialize frontmatter with fields in specified order.
fn serialize_frontmatter_ordered(
    output: &mut String,
    fm: &super::Frontmatter,
    field_order: &[&str],
) {
    // Output fields in schema order
    for field in field_order {
        // For schema-driven docs, always check extra first since normalization stores there
        if let Some(value) = fm.extra.get(*field) {
            serialize_yaml_field(output, field, value, 0);
            continue;
        }

        // Fall back to known fields
        match *field {
            "created" => {
                if let Some(created) = &fm.created {
                    output.push_str(&format!("created: {}\n", created.format("%Y-%m-%d")));
                }
            }
            "description" => {
                if let Some(description) = &fm.description {
                    output.push_str(&format!("description: {description}\n"));
                }
            }
            "name" => {
                if let Some(name) = &fm.name {
                    output.push_str(&format!("name: {name}\n"));
                }
            }
            "type" => {
                if let Some(doc_type) = &fm.doc_type {
                    output.push_str(&format!("type: {doc_type}\n"));
                }
            }
            _ => {}
        }
    }
}

/// Serialize a YAML field with proper formatting.
fn serialize_yaml_field(output: &mut String, key: &str, value: &serde_yaml::Value, indent: usize) {
    let indent_str = "  ".repeat(indent);

    match value {
        serde_yaml::Value::Null => {
            output.push_str(&format!("{indent_str}{key}:\n"));
        }
        serde_yaml::Value::Bool(b) => {
            output.push_str(&format!("{indent_str}{key}: {b}\n"));
        }
        serde_yaml::Value::Number(n) => {
            output.push_str(&format!("{indent_str}{key}: {n}\n"));
        }
        serde_yaml::Value::String(s) => {
            // Check if string needs quoting
            if needs_yaml_quoting(s) {
                output.push_str(&format!(
                    "{indent_str}{key}: \"{}\"\n",
                    escape_yaml_string(s)
                ));
            } else {
                output.push_str(&format!("{indent_str}{key}: {s}\n"));
            }
        }
        serde_yaml::Value::Sequence(seq) => {
            if seq.is_empty() {
                output.push_str(&format!("{indent_str}{key}: []\n"));
            } else {
                output.push_str(&format!("{indent_str}{key}:\n"));
                for item in seq {
                    serialize_yaml_list_item(output, item, indent + 1);
                }
            }
        }
        serde_yaml::Value::Mapping(map) => {
            output.push_str(&format!("{indent_str}{key}:\n"));
            for (k, v) in map {
                if let serde_yaml::Value::String(k_str) = k {
                    serialize_yaml_field(output, k_str, v, indent + 1);
                }
            }
        }
        serde_yaml::Value::Tagged(_) => {
            // Skip tagged values
        }
    }
}

/// Serialize a YAML list item.
fn serialize_yaml_list_item(output: &mut String, value: &serde_yaml::Value, indent: usize) {
    let indent_str = "  ".repeat(indent);

    match value {
        serde_yaml::Value::String(s) => {
            if needs_yaml_quoting(s) {
                output.push_str(&format!("{indent_str}- \"{}\"\n", escape_yaml_string(s)));
            } else {
                output.push_str(&format!("{indent_str}- {s}\n"));
            }
        }
        serde_yaml::Value::Number(n) => {
            output.push_str(&format!("{indent_str}- {n}\n"));
        }
        serde_yaml::Value::Bool(b) => {
            output.push_str(&format!("{indent_str}- {b}\n"));
        }
        _ => {
            // For complex values, just use serde_yaml's to_string
            if let Ok(yaml) = serde_yaml::to_string(value) {
                for line in yaml.trim().lines() {
                    output.push_str(&format!("{indent_str}- {line}\n"));
                }
            }
        }
    }
}

/// Check if a string needs YAML quoting.
fn needs_yaml_quoting(s: &str) -> bool {
    s.is_empty()
        || s.starts_with(' ')
        || s.ends_with(' ')
        || s.contains(':')
        || s.contains('#')
        || s.contains('\n')
        || s.contains('"')
        || s.contains('\'')
        || s.starts_with('!')
        || s.starts_with('&')
        || s.starts_with('*')
        || s == "true"
        || s == "false"
        || s == "null"
        || s == "~"
        || s.parse::<f64>().is_ok()
}

/// Escape a string for YAML double-quoted output.
fn escape_yaml_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_heading() {
        let doc = parse("# Hello World\n");
        assert_eq!(doc.blocks.len(), 1);
        assert!(matches!(&doc.blocks[0], Block::Heading { level: 1, .. }));
        assert_eq!(
            doc.blocks[0].heading_text(),
            Some("Hello World".to_string())
        );
    }

    #[test]
    fn test_parse_list() {
        let doc = parse("- Item 1\n- Item 2\n");
        assert_eq!(doc.blocks.len(), 1);
        if let Block::List { items, ordered } = &doc.blocks[0] {
            assert!(!ordered);
            assert_eq!(items.len(), 2);
        } else {
            panic!("Expected list");
        }
    }

    #[test]
    fn test_parse_code_block() {
        let doc = parse("```rust\nfn main() {}\n```\n");
        assert_eq!(doc.blocks.len(), 1);
        if let Block::CodeBlock { language, content } = &doc.blocks[0] {
            assert_eq!(language.as_deref(), Some("rust"));
            assert_eq!(content, "fn main() {}\n");
        } else {
            panic!("Expected code block");
        }
    }

    #[test]
    fn test_parse_frontmatter() {
        let content = r#"---
created: 2024-03-15
description: Test project
---
# Title
"#;
        let doc = parse(content);
        assert!(doc.frontmatter.is_some());
        let fm = doc.frontmatter.as_ref().unwrap();
        assert!(fm.created.is_some());
        assert_eq!(fm.description.as_deref(), Some("Test project"));
    }

    #[test]
    fn test_serialize_roundtrip() {
        let content = r#"---
created: 2024-03-15
description: Test project
---

# Title

Some paragraph text.

- Item 1
- Item 2

```bash
echo hello
```
"#;
        let doc = parse(content);
        let serialized = serialize(&doc);
        let doc2 = parse(&serialized);
        let serialized2 = serialize(&doc2);

        assert_eq!(serialized, serialized2, "Should be idempotent");
    }

    #[test]
    fn test_blank_line_after_heading() {
        let doc = parse("# Title\nParagraph right after\n");
        assert!(doc.blocks.len() >= 2);
        assert!(matches!(&doc.blocks[0], Block::Heading { .. }));
        assert!(matches!(&doc.blocks[1], Block::BlankLine));
    }

    #[test]
    fn test_parse_link() {
        let doc = parse("Check out [this link](https://example.com)\n");
        if let Block::Paragraph(inlines) = &doc.blocks[0] {
            let has_link = inlines.iter().any(|i| {
                matches!(i, Inline::Link { text, url } if text == "this link" && url == "https://example.com")
            });
            assert!(has_link, "Should contain link");
        } else {
            panic!("Expected paragraph");
        }
    }

    #[test]
    fn test_nested_list_roundtrip() {
        let content = r#"1. **First item** - with description:
   - sub item a
   - sub item b

2. **Second item** - another:
   - sub item c
   - sub item d
"#;
        let doc = parse(content);

        // Verify structure
        if let Block::List { items, ordered } = &doc.blocks[0] {
            assert!(ordered);
            assert_eq!(items.len(), 2);

            // First item should have nested list
            assert!(!items[0].children.is_empty());
            if let Block::List {
                items: sub_items,
                ordered: sub_ordered,
            } = &items[0].children[0]
            {
                assert!(!sub_ordered);
                assert_eq!(sub_items.len(), 2);
            } else {
                panic!("Expected nested list");
            }
        } else {
            panic!("Expected list");
        }

        // Verify serialization preserves nested structure with indentation
        let serialized = serialize(&doc);
        assert!(serialized.contains("1. **First item**"));
        assert!(serialized.contains("   - sub item a"));
        assert!(serialized.contains("   - sub item b"));
        assert!(serialized.contains("2. **Second item**"));
        assert!(serialized.contains("   - sub item c"));
        assert!(serialized.contains("   - sub item d"));
    }

    #[test]
    fn test_bold_italic_roundtrip() {
        let content = "This has **bold** and *italic* and ***both*** text.\n";
        let doc = parse(content);
        let serialized = serialize(&doc);

        assert!(serialized.contains("**bold**"));
        assert!(serialized.contains("*italic*"));
        assert!(serialized.contains("***both***") || serialized.contains("***both***"));
    }

    #[test]
    fn test_is_encrypted_true() {
        // Note: key-id must be quoted because 0xABCD1234 is parsed as an integer otherwise
        let content = r#"---
type: health/weight-log
encrypted: true
key-id: "0xABCD1234"
wrapped-key: dGVzdGtleQ==
---
dGVzdGJvZHk=
"#;
        assert!(is_encrypted(content));

        // Also verify frontmatter fields are parsed correctly
        let doc = parse(content);
        let fm = doc.frontmatter.as_ref().expect("should have frontmatter");
        assert!(fm.encrypted);
        assert_eq!(fm.key_id.as_deref(), Some("0xABCD1234"));
        assert_eq!(fm.wrapped_key.as_deref(), Some("dGVzdGtleQ=="));
    }

    #[test]
    fn test_is_encrypted_false() {
        let content = r#"---
type: health/weight-log
---
# Weight Log

Some content here.
"#;
        assert!(!is_encrypted(content));
    }

    #[test]
    fn test_is_encrypted_false_no_frontmatter() {
        let content = "# Just a heading\n\nSome content.\n";
        assert!(!is_encrypted(content));
    }

    #[test]
    fn test_serialize_with_field_order_preserves_extra_fields() {
        let content = r#"---
type: target
category: account
priority: high
---

# 1Password
"#;
        let doc = parse(content);

        // Simulate what format.rs does: build field_order from structure.frontmatter
        let field_order = vec!["type", "category", "priority"];
        let serialized = serialize_with_field_order(&doc, Some(&field_order));

        // The key assertion: category and priority should be preserved
        assert!(
            serialized.contains("category: account"),
            "category field should be preserved, got:\n{}",
            serialized
        );
        assert!(
            serialized.contains("priority: high"),
            "priority field should be preserved, got:\n{}",
            serialized
        );
        assert!(
            serialized.contains("type: target"),
            "type field should be preserved, got:\n{}",
            serialized
        );
    }

    #[test]
    fn test_serialize_with_field_order_respects_order() {
        let content = r#"---
type: target
zebra: z
alpha: a
---

# Test
"#;
        let doc = parse(content);

        // Order should be: type, alpha, zebra (not alphabetical)
        let field_order = vec!["type", "alpha", "zebra"];
        let serialized = serialize_with_field_order(&doc, Some(&field_order));

        // Check that alpha comes before zebra in the output
        let alpha_pos = serialized.find("alpha:").expect("should have alpha");
        let zebra_pos = serialized.find("zebra:").expect("should have zebra");
        assert!(
            alpha_pos < zebra_pos,
            "alpha should come before zebra in output:\n{}",
            serialized
        );
    }
}

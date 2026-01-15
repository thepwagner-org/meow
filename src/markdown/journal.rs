//! Journal file parsing, validation, and formatting.
//!
//! Journal files live at `projects/{name}/journal/YYYY-MM.md` and contain
//! work logs organized by date.

use super::{
    inlines_to_string, is_encrypted, parse, serialize, validate_links, Block, Document,
    FormatContext, Inline, ValidationError,
};
use crate::{JOURNAL_DIR, PROJECTS_DIR};
use anyhow::{Context, Result};
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use std::cmp::Reverse;
use std::fs;
use std::path::Path;

/// Validate a journal document.
pub fn validate(doc: &Document, ctx: &FormatContext) -> Vec<ValidationError> {
    let mut errors = Vec::new();

    let year_month = ctx.year_month.as_deref().unwrap_or("unknown");

    // Check H1 is a month name
    let has_valid_h1 = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::Heading { level: 1, .. }));

    if !has_valid_h1 {
        errors.push(ValidationError {
            line: 1,
            message: "journal must have a month heading (# Month Year)".to_string(),
        });
    }

    // Check H3s are valid dates (with optional time)
    for block in &doc.blocks {
        if let Block::Heading { level: 3, content } = block {
            let text = inlines_to_string(content);
            if let Some((date, _time)) = parse_journal_heading(&text) {
                // Check date matches year_month
                let date_str = date.format("%Y-%m-%d").to_string();
                if !date_str.starts_with(year_month) {
                    errors.push(ValidationError {
                        line: 0,
                        message: format!("date {text} doesn't match file {year_month}"),
                    });
                }
            } else {
                errors.push(ValidationError {
                    line: 0,
                    message: format!(
                        "invalid date format: {text:?} (expected YYYY-MM-DD or YYYY-MM-DD HH:MM)"
                    ),
                });
            }
        }

        // Check for malformed headings (parsed as paragraphs because missing space after #)
        if let Block::Paragraph(content) = block {
            let text = inlines_to_string(content);
            if text.starts_with("###") {
                errors.push(ValidationError {
                    line: 0,
                    message: format!("malformed heading: {text:?} (missing space after ###)"),
                });
            }
        }
    }

    // Validate links
    errors.extend(validate_links(doc, ctx));

    errors
}

/// Normalize a journal document in place.
pub fn normalize(doc: &mut Document, ctx: &FormatContext) {
    let year_month = ctx.year_month.as_deref().unwrap_or("unknown");

    // Ensure H1 month heading exists and is correct
    let month_name = generate_month_name(year_month);
    let has_h1 = doc
        .blocks
        .iter()
        .any(|b| matches!(b, Block::Heading { level: 1, .. }));

    if !has_h1 {
        doc.blocks.insert(
            0,
            Block::Heading {
                level: 1,
                content: vec![Inline::Text(month_name)],
            },
        );
    }

    // Parse and sort entries
    let ParsedJournal {
        preamble,
        mut entries,
    } = parse_journal_blocks(doc.blocks.drain(..));

    entries.sort_by_key(|(date, time, _)| Reverse((*date, time.unwrap_or_else(default_sort_time))));

    // Rebuild document
    doc.blocks = preamble;
    for (_, _, blocks) in entries {
        doc.blocks.extend(blocks);
    }
}

/// Generate a month name from YYYY-MM format.
fn generate_month_name(year_month: &str) -> String {
    if let Some((year, month)) = year_month.split_once('-') {
        let month_name = match month {
            "01" => "January",
            "02" => "February",
            "03" => "March",
            "04" => "April",
            "05" => "May",
            "06" => "June",
            "07" => "July",
            "08" => "August",
            "09" => "September",
            "10" => "October",
            "11" => "November",
            "12" => "December",
            _ => return year_month.to_string(),
        };
        format!("{month_name} {year}")
    } else {
        year_month.to_string()
    }
}

/// Result of parsing journal blocks: preamble (before entries) and entries.
struct ParsedJournal {
    preamble: Vec<Block>,
    entries: Vec<(NaiveDate, Option<NaiveTime>, Vec<Block>)>,
}

/// Parse blocks into preamble and entries.
/// Entries are (date, optional_time, blocks including H3 heading).
fn parse_journal_blocks(blocks: impl IntoIterator<Item = Block>) -> ParsedJournal {
    let mut preamble = Vec::new();
    let mut entries = Vec::new();
    let mut current: Option<(NaiveDate, Option<NaiveTime>)> = None;
    let mut current_blocks: Vec<Block> = Vec::new();
    let mut in_preamble = true;

    for block in blocks {
        if let Block::Heading {
            level: 3,
            ref content,
        } = block
        {
            // Save previous entry
            if let Some((date, time)) = current.take() {
                entries.push((date, time, std::mem::take(&mut current_blocks)));
            }

            let text = inlines_to_string(content);
            if let Some((date, time)) = parse_journal_heading(&text) {
                current = Some((date, time));
                current_blocks.push(block);
                in_preamble = false;
            } else {
                current_blocks.push(block);
            }
        } else if in_preamble {
            preamble.push(block);
        } else {
            current_blocks.push(block);
        }
    }

    // Save last entry
    if let Some((date, time)) = current {
        entries.push((date, time, current_blocks));
    }

    ParsedJournal { preamble, entries }
}

/// A single journal entry (date + optional time + content blocks).
#[derive(Debug)]
pub struct JournalEntry {
    pub date: NaiveDate,
    /// Optional time; if None, sorts as 23:59 (end of day).
    pub time: Option<NaiveTime>,
    pub blocks: Vec<Block>,
}

/// A git commit for timeline display.
#[derive(Debug)]
pub struct CommitEntry {
    pub datetime: NaiveDateTime,
    pub subject: String,
    /// Full body text, already sanitized (no heading prefixes).
    pub body: Option<String>,
    pub short_hash: String,
}

/// A timeline item that can be either a journal entry or a git commit.
#[derive(Debug)]
pub enum TimelineItem {
    Journal(JournalEntry),
    Commit(CommitEntry),
}

impl TimelineItem {
    /// Get the datetime for sorting (newest first).
    pub fn datetime(&self) -> NaiveDateTime {
        match self {
            TimelineItem::Journal(e) => {
                let time = e.time.unwrap_or_else(default_sort_time);
                e.date.and_time(time)
            }
            TimelineItem::Commit(c) => c.datetime,
        }
    }
}

/// Default time for entries without explicit timestamp (end of day for sorting).
fn default_sort_time() -> NaiveTime {
    // SAFETY: 23:59:00 is always a valid time
    #[allow(clippy::expect_used)]
    NaiveTime::from_hms_opt(23, 59, 0).expect("23:59:00 is a valid time")
}

/// Parse a journal heading, returning (date, optional time).
/// Accepts "YYYY-MM-DD" or "YYYY-MM-DD HH:MM".
fn parse_journal_heading(text: &str) -> Option<(NaiveDate, Option<NaiveTime>)> {
    // Try datetime first: "2025-11-29 14:30"
    if text.len() > 10 && text.chars().nth(10) == Some(' ') {
        let (date_part, time_part) = text.split_at(10);
        let time_part = time_part.trim_start();
        if let Ok(date) = NaiveDate::parse_from_str(date_part, "%Y-%m-%d") {
            if let Ok(time) = NaiveTime::parse_from_str(time_part, "%H:%M") {
                return Some((date, Some(time)));
            }
        }
    }

    // Fall back to date only: "2025-11-29"
    if let Ok(date) = NaiveDate::parse_from_str(text, "%Y-%m-%d") {
        return Some((date, None));
    }

    None
}

/// Read journal entries for a project, optionally filtered by date range.
pub fn read_journal(
    root: &Path,
    project: &str,
    since: Option<NaiveDate>,
    until: Option<NaiveDate>,
) -> Result<Vec<JournalEntry>> {
    let journal_dir = root.join(PROJECTS_DIR).join(project).join(JOURNAL_DIR);

    if !journal_dir.exists() {
        return Ok(vec![]);
    }

    let mut entries = Vec::new();

    // Read all YYYY-MM.md files
    let read_dir = fs::read_dir(&journal_dir).context("Failed to read journal directory")?;
    for entry in read_dir.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().is_none_or(|e| e != "md") {
            continue;
        }

        let content = fs::read_to_string(&path).context("Failed to read journal file")?;

        // Skip encrypted journal files
        if is_encrypted(&content) {
            continue;
        }

        let doc = parse(&content);

        // Extract entries from this file
        let file_entries = extract_entries(&doc);
        entries.extend(file_entries);
    }

    // Filter by date range
    entries.retain(|e| {
        let after_since = since.is_none_or(|s| e.date >= s);
        let before_until = until.is_none_or(|u| e.date <= u);
        after_since && before_until
    });

    // Sort by datetime descending (newest first)
    // Entries without time sort as 23:59 (end of day)
    entries.sort_by_key(|e| Reverse((e.date, e.time.unwrap_or_else(default_sort_time))));

    Ok(entries)
}

/// Extract journal entries from a parsed document.
fn extract_entries(doc: &Document) -> Vec<JournalEntry> {
    let parsed = parse_journal_blocks(doc.blocks.iter().cloned());
    parsed
        .entries
        .into_iter()
        .map(|(date, time, blocks)| JournalEntry { date, time, blocks })
        .collect()
}

/// Render journal entries as markdown for display.
pub fn render_entries(project: &str, entries: &[JournalEntry]) -> String {
    let mut out = String::new();

    // Project name as header
    out.push_str(&format!("# {project}\n\n"));

    for entry in entries {
        // Date (and optional time) as H2
        let heading = match entry.time {
            Some(time) => format!("{} {}", entry.date.format("%Y-%m-%d"), time.format("%H:%M")),
            None => entry.date.format("%Y-%m-%d").to_string(),
        };
        out.push_str(&format!("## {heading}\n"));

        // Render content blocks (skip the H3 date heading and blank lines)
        for block in &entry.blocks {
            match block {
                Block::Heading { level: 3, .. } | Block::BlankLine => {
                    // Skip - we already rendered the date, and control spacing ourselves
                }
                _ => {
                    let mini_doc = Document {
                        frontmatter: None,
                        blocks: vec![block.clone()],
                    };
                    out.push_str(&serialize(&mini_doc));
                }
            }
        }
        out.push('\n');
    }

    out.trim_end().to_string()
}

/// Render a timeline (interleaved journal entries and commits) as markdown.
pub fn render_timeline(project: &str, items: &[TimelineItem]) -> String {
    let mut out = String::new();

    // Project name as header
    out.push_str(&format!("# {project}\n\n"));

    for item in items {
        match item {
            TimelineItem::Journal(entry) => {
                // Date (and optional time) as H2
                let heading = match entry.time {
                    Some(time) => {
                        format!("{} {}", entry.date.format("%Y-%m-%d"), time.format("%H:%M"))
                    }
                    None => entry.date.format("%Y-%m-%d").to_string(),
                };
                out.push_str(&format!("## {heading}\n"));

                // Render content blocks (skip the H3 date heading and blank lines)
                for block in &entry.blocks {
                    match block {
                        Block::Heading { level: 3, .. } | Block::BlankLine => {
                            // Skip - we already rendered the date, and control spacing ourselves
                        }
                        _ => {
                            let mini_doc = Document {
                                frontmatter: None,
                                blocks: vec![block.clone()],
                            };
                            out.push_str(&serialize(&mini_doc));
                        }
                    }
                }
                out.push('\n');
            }
            TimelineItem::Commit(commit) => {
                // Commits render as H2 with [commit] suffix to distinguish from journal entries
                let heading = commit.datetime.format("%Y-%m-%d %H:%M").to_string();
                out.push_str(&format!("## {heading} [commit]\n"));

                // Subject as first paragraph
                out.push_str(&commit.subject);
                out.push_str("\n\n");

                // Body if present
                if let Some(body) = &commit.body {
                    let trimmed = body.trim();
                    if !trimmed.is_empty() {
                        out.push_str(trimmed);
                        out.push_str("\n\n");
                    }
                }
            }
        }
    }

    out.trim_end().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::{parse, serialize};

    fn make_ctx(year_month: &str) -> FormatContext<'_> {
        FormatContext {
            project: "test",
            path: Path::new("journal/2025-11.md"),
            year_month: Some(year_month.to_string()),
            git_tree: None,
            repo_root: None,
        }
    }

    #[test]
    fn test_parse_and_format_journal() {
        let content = r#"# November 2025

### 2025-11-28
- Earlier entry

### 2025-11-29
- Later entry
"#;
        let mut doc = parse(content);
        normalize(&mut doc, &make_ctx("2025-11"));
        let formatted = serialize(&doc);

        // 2025-11-29 should come before 2025-11-28 (newest first)
        let pos_29 = formatted.find("2025-11-29").expect("29");
        let pos_28 = formatted.find("2025-11-28").expect("28");
        assert!(pos_29 < pos_28, "newer date should come first");
    }

    #[test]
    fn test_validate_invalid_date() {
        let content = r#"# November 2025

### Nov 29
- Bad date format
"#;
        let doc = parse(content);
        let errors = validate(&doc, &make_ctx("2025-11"));
        assert!(errors.iter().any(|e| e.message.contains("invalid date")));
    }

    #[test]
    fn test_validate_malformed_heading() {
        // Missing space after ### - CommonMark parses this as a paragraph, not a heading
        let content = r#"# November 2025

### 2025-11-30
Good entry.

###2025-11-30 broken entry
Bad entry.
"#;
        let doc = parse(content);
        let errors = validate(&doc, &make_ctx("2025-11"));
        assert!(
            errors
                .iter()
                .any(|e| e.message.contains("malformed heading")),
            "should detect malformed heading: {errors:?}"
        );
    }

    #[test]
    fn test_generate_month_name() {
        assert_eq!(generate_month_name("2025-11"), "November 2025");
        assert_eq!(generate_month_name("2025-01"), "January 2025");
        assert_eq!(generate_month_name("invalid"), "invalid");
    }

    #[test]
    fn test_parse_journal_heading() {
        // Date only
        let (date, time) = parse_journal_heading("2025-11-29").expect("valid");
        assert_eq!(
            date,
            NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid date")
        );
        assert!(time.is_none());

        // Date with time
        let (date, time) = parse_journal_heading("2025-11-29 14:30").expect("valid");
        assert_eq!(
            date,
            NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid date")
        );
        assert_eq!(
            time,
            Some(NaiveTime::from_hms_opt(14, 30, 0).expect("valid time"))
        );

        // Invalid formats
        assert!(parse_journal_heading("Nov 29").is_none());
        assert!(parse_journal_heading("2025-11-29 25:00").is_none()); // invalid hour
        assert!(parse_journal_heading("2025-11-29 14").is_none()); // missing minutes
    }

    #[test]
    fn test_multiple_same_day_entries_with_times() {
        let content = r#"# November 2025

### 2025-11-29 09:00
- Morning work

### 2025-11-29 14:30
- Afternoon work

### 2025-11-29
- Entry without time (sorts last for the day)
"#;
        let mut doc = parse(content);
        normalize(&mut doc, &make_ctx("2025-11"));
        let formatted = serialize(&doc);

        // Entry without time (23:59) should come first, then 14:30, then 09:00
        let pos_no_time = formatted.find("### 2025-11-29\n").expect("no time");
        let pos_14_30 = formatted.find("### 2025-11-29 14:30").expect("14:30");
        let pos_09_00 = formatted.find("### 2025-11-29 09:00").expect("09:00");
        assert!(
            pos_no_time < pos_14_30,
            "no-time entry should come before 14:30"
        );
        assert!(pos_14_30 < pos_09_00, "14:30 should come before 09:00");
    }

    #[test]
    fn test_validate_accepts_timestamps() {
        let content = r#"# November 2025

### 2025-11-29 14:30
- Entry with time
"#;
        let doc = parse(content);
        let errors = validate(&doc, &make_ctx("2025-11"));
        assert!(errors.is_empty(), "timestamps should be valid: {errors:?}");
    }

    #[test]
    fn test_render_entries_with_time() {
        let entries = vec![
            JournalEntry {
                date: NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid"),
                time: Some(NaiveTime::from_hms_opt(14, 30, 0).expect("valid")),
                blocks: vec![],
            },
            JournalEntry {
                date: NaiveDate::from_ymd_opt(2025, 11, 28).expect("valid"),
                time: None,
                blocks: vec![],
            },
        ];
        let rendered = render_entries("test", &entries);
        assert!(
            rendered.contains("## 2025-11-29 14:30"),
            "should have time: {rendered}"
        );
        // No trailing newline due to trim_end(), so just check for the heading without time
        assert!(
            rendered.contains("## 2025-11-28"),
            "should not have time: {rendered}"
        );
        assert!(
            !rendered.contains("## 2025-11-28 "),
            "date-only should not have trailing space"
        );
    }
}

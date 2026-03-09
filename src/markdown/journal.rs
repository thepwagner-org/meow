//! Journal file reading and rendering.
//!
//! Journal files live at `projects/{name}/journal/YYYY-MM.md` and contain
//! work logs organized by date (`## YYYY-MM-DD` headings).

use crate::{JOURNAL_DIR, PROJECTS_DIR};
use anyhow::{Context, Result};
use chrono::{NaiveDate, NaiveDateTime, NaiveTime};
use std::cmp::Reverse;
use std::fs;
use std::path::Path;

/// A single journal entry (date + optional time + raw markdown content).
#[derive(Debug)]
pub struct JournalEntry {
    pub date: NaiveDate,
    /// Optional time; if None, sorts as 23:59 (end of day).
    pub time: Option<NaiveTime>,
    /// Raw markdown content (everything after the heading line).
    pub content: String,
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
/// Accepts "YYYY-MM-DD", "YYYY-MM-DD HH:MM", or "YYYY-MM-DD HH:MM - Title".
fn parse_journal_heading(text: &str) -> Option<(NaiveDate, Option<NaiveTime>)> {
    let text = text.trim();

    if text.len() > 10 && text.as_bytes().get(10) == Some(&b' ') {
        let (date_part, rest) = text.split_at(10);
        let date = NaiveDate::parse_from_str(date_part, "%Y-%m-%d").ok()?;
        let first_token = rest.trim_start().split_whitespace().next().unwrap_or("");

        // If the next token is "-", this is a titled date-only heading
        // (e.g. "2026-02-06 - 1:1 with Kaiyi") — date only.
        if first_token == "-" {
            return Some((date, None));
        }

        // Otherwise treat it as a time token and require it to be valid HH:MM.
        // This rejects malformed entries like "2025-11-29 25:00" or "2025-11-29 14"
        // rather than silently falling back to date-only.
        let time = NaiveTime::parse_from_str(first_token, "%H:%M").ok()?;
        return Some((date, Some(time)));
    }

    // Plain date: "2025-11-29"
    if let Ok(date) = NaiveDate::parse_from_str(text, "%Y-%m-%d") {
        return Some((date, None));
    }

    None
}

/// Extract journal entries from raw markdown content.
fn extract_entries(content: &str) -> Vec<JournalEntry> {
    let mut entries = Vec::new();
    let mut current_date: Option<NaiveDate> = None;
    let mut current_time: Option<NaiveTime> = None;
    let mut current_lines: Vec<&str> = Vec::new();

    for line in content.lines() {
        if let Some(heading_text) = line.strip_prefix("## ") {
            // Flush previous entry
            if let Some(date) = current_date.take() {
                let entry_content = drain_content(&mut current_lines);
                entries.push(JournalEntry {
                    date,
                    time: current_time.take(),
                    content: entry_content,
                });
            }

            // Try to parse as date heading
            if let Some((date, time)) = parse_journal_heading(heading_text) {
                current_date = Some(date);
                current_time = time;
            }
        } else if current_date.is_some() {
            current_lines.push(line);
        }
    }

    // Flush last entry
    if let Some(date) = current_date {
        let entry_content = drain_content(&mut current_lines);
        entries.push(JournalEntry {
            date,
            time: current_time,
            content: entry_content,
        });
    }

    entries
}

/// Collapse accumulated lines into trimmed content.
fn drain_content(lines: &mut Vec<&str>) -> String {
    // Strip leading/trailing blank lines
    while lines.first().is_some_and(|l| l.trim().is_empty()) {
        let _ = lines.remove(0);
    }
    while lines.last().is_some_and(|l| l.trim().is_empty()) {
        let _ = lines.pop();
    }
    let s = lines.join("\n");
    lines.clear();
    s
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

    let read_dir = fs::read_dir(&journal_dir).context("Failed to read journal directory")?;
    for entry in read_dir.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().is_none_or(|e| e != "md") {
            continue;
        }

        let content = fs::read_to_string(&path).context("Failed to read journal file")?;
        entries.extend(extract_entries(&content));
    }

    // Filter by date range
    entries.retain(|e| {
        let after_since = since.is_none_or(|s| e.date >= s);
        let before_until = until.is_none_or(|u| e.date <= u);
        after_since && before_until
    });

    // Sort by datetime descending (newest first)
    entries.sort_by_key(|e| Reverse((e.date, e.time.unwrap_or_else(default_sort_time))));

    Ok(entries)
}

/// Render journal entries as markdown for display.
pub fn render_entries(project: &str, entries: &[JournalEntry]) -> String {
    let mut out = String::new();
    out.push_str(&format!("# {project}\n\n"));

    for entry in entries {
        let heading = format_heading(entry.date, entry.time);
        out.push_str(&format!("## {heading}\n"));
        if !entry.content.is_empty() {
            out.push_str(&entry.content);
            out.push('\n');
        }
        out.push('\n');
    }

    out.trim_end().to_string()
}

/// Render a timeline (interleaved journal entries and commits) as markdown.
pub fn render_timeline(project: &str, items: &[TimelineItem]) -> String {
    let mut out = String::new();
    out.push_str(&format!("# {project}\n\n"));

    for item in items {
        match item {
            TimelineItem::Journal(entry) => {
                let heading = format_heading(entry.date, entry.time);
                out.push_str(&format!("## {heading}\n"));
                if !entry.content.is_empty() {
                    out.push_str(&entry.content);
                    out.push('\n');
                }
                out.push('\n');
            }
            TimelineItem::Commit(commit) => {
                let heading = commit.datetime.format("%Y-%m-%d %H:%M").to_string();
                out.push_str(&format!("## {heading} [commit]\n"));
                out.push_str(&commit.subject);
                out.push_str("\n\n");

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

fn format_heading(date: NaiveDate, time: Option<NaiveTime>) -> String {
    match time {
        Some(t) => format!("{} {}", date.format("%Y-%m-%d"), t.format("%H:%M")),
        None => date.format("%Y-%m-%d").to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_journal_heading_date_only() {
        let (date, time) = parse_journal_heading("2025-11-29").expect("valid");
        assert_eq!(date, NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid"));
        assert!(time.is_none());
    }

    #[test]
    fn test_parse_journal_heading_with_time() {
        let (date, time) = parse_journal_heading("2025-11-29 14:30").expect("valid");
        assert_eq!(date, NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid"));
        assert_eq!(
            time,
            Some(NaiveTime::from_hms_opt(14, 30, 0).expect("valid"))
        );
    }

    #[test]
    fn test_parse_journal_heading_with_title() {
        // "YYYY-MM-DD HH:MM - Title" should parse date+time, ignoring the title
        let (date, time) = parse_journal_heading("2026-02-02 08:24 - Vibe Check").expect("valid");
        assert_eq!(date, NaiveDate::from_ymd_opt(2026, 2, 2).expect("valid"));
        assert_eq!(
            time,
            Some(NaiveTime::from_hms_opt(8, 24, 0).expect("valid"))
        );
    }

    #[test]
    fn test_parse_journal_heading_date_with_title() {
        // "YYYY-MM-DD - Title" should parse date only, ignoring the title
        let (date, time) = parse_journal_heading("2026-02-06 - 1:1 with Kaiyi").expect("valid");
        assert_eq!(date, NaiveDate::from_ymd_opt(2026, 2, 6).expect("valid"));
        assert!(time.is_none());
    }

    #[test]
    fn test_parse_journal_heading_invalid() {
        assert!(parse_journal_heading("Nov 29").is_none());
        assert!(parse_journal_heading("2025-11-29 25:00").is_none());
        assert!(parse_journal_heading("2025-11-29 14").is_none());
    }

    #[test]
    fn test_extract_entries() {
        let content =
            "# November 2025\n\n## 2025-11-29\n- Later entry\n\n## 2025-11-28\n- Earlier entry\n";
        let entries = extract_entries(content);
        assert_eq!(entries.len(), 2);
        assert_eq!(
            entries[0].date,
            NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid")
        );
        assert!(entries[0].content.contains("Later entry"));
        assert_eq!(
            entries[1].date,
            NaiveDate::from_ymd_opt(2025, 11, 28).expect("valid")
        );
        assert!(entries[1].content.contains("Earlier entry"));
    }

    #[test]
    fn test_extract_entries_with_time() {
        let content = "# Nov 2025\n\n## 2025-11-29 14:30\n- Afternoon work\n\n## 2025-11-29 09:00\n- Morning work\n";
        let entries = extract_entries(content);
        assert_eq!(entries.len(), 2);
        assert!(entries[0].time.is_some());
        assert!(entries[1].time.is_some());
    }

    #[test]
    fn test_render_entries() {
        let entries = vec![
            JournalEntry {
                date: NaiveDate::from_ymd_opt(2025, 11, 29).expect("valid"),
                time: Some(NaiveTime::from_hms_opt(14, 30, 0).expect("valid")),
                content: "- Afternoon work".to_string(),
            },
            JournalEntry {
                date: NaiveDate::from_ymd_opt(2025, 11, 28).expect("valid"),
                time: None,
                content: "- Earlier entry".to_string(),
            },
        ];
        let rendered = render_entries("test", &entries);
        assert!(rendered.contains("## 2025-11-29 14:30"));
        assert!(rendered.contains("## 2025-11-28\n"));
        assert!(!rendered.contains("## 2025-11-28 "));
    }
}

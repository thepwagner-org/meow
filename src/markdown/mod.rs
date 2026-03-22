//! Lightweight markdown frontmatter extraction and journal reading.

pub mod journal;
mod style;

pub use journal::{
    read_journal, render_entries, render_timeline, CommitEntry, JournalEntry, TimelineItem,
};
pub use style::skin;

use crate::{git, PROJECTS_DIR};
use chrono::NaiveDate;
use git2::Repository;
use gray_matter::{engine::YAML, Matter};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// How a project should be indexed by QMD.
#[derive(Debug, Clone, PartialEq)]
pub enum QmdMode {
    /// Add collection, do not auto-embed.
    Index,
    /// Add collection and run `qmd embed` after sync.
    Embed,
}

impl QmdMode {
    /// Parse from a serde_yaml::Value. Treats null/missing/false as None.
    pub fn from_value(v: &serde_yaml::Value) -> Option<Self> {
        match v {
            serde_yaml::Value::String(s) => match s.as_str() {
                "embed" => Some(Self::Embed),
                "true" => Some(Self::Index),
                _ => None,
            },
            serde_yaml::Value::Bool(true) => Some(Self::Index),
            _ => None,
        }
    }
}

/// YAML frontmatter fields.
#[derive(Debug, Clone, Deserialize, Default)]
pub struct Frontmatter {
    #[serde(default, deserialize_with = "deserialize_date")]
    pub created: Option<NaiveDate>,
    pub description: Option<String>,
    pub name: Option<String>,
    /// All other frontmatter fields.
    #[serde(flatten)]
    pub extra: HashMap<String, serde_yaml::Value>,
}

impl Frontmatter {
    /// Returns the QMD indexing mode if configured.
    pub fn qmd_mode(&self) -> Option<QmdMode> {
        self.extra.get("qmd").and_then(QmdMode::from_value)
    }
}

/// Extract frontmatter from markdown content.
pub fn parse_frontmatter(content: &str) -> Option<Frontmatter> {
    let matter = Matter::<YAML>::new();
    let parsed = matter.parse(content);
    parsed.data.and_then(|d| d.deserialize().ok())
}

/// Load frontmatter from a project's README (filesystem first, git fallback).
pub fn load_readme_frontmatter(
    repo: &Repository,
    root: &Path,
    project: &str,
) -> Option<Frontmatter> {
    let fs_path = root.join(PROJECTS_DIR).join(project).join("README.md");
    let content = if fs_path.exists() {
        fs::read_to_string(&fs_path).ok()
    } else {
        let git_path = format!("{}/{}/README.md", PROJECTS_DIR, project);
        git::read_blob(repo, &git_path).ok()
    };
    content.and_then(|c| parse_frontmatter(&c))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_frontmatter_basic() {
        let text = "---\ncreated: 2024-03-15\ndescription: A test project\n---\n# Test\n";
        let fm = parse_frontmatter(text).expect("frontmatter");
        assert_eq!(
            fm.created,
            Some(NaiveDate::from_ymd_opt(2024, 3, 15).expect("valid date"))
        );
        assert_eq!(fm.description.as_deref(), Some("A test project"));
    }

    #[test]
    fn test_parse_frontmatter_extra_fields() {
        let text = "---\ncreated: 2024-01-01\ngithub: org/repo\n---\n# Test\n";
        let fm = parse_frontmatter(text).expect("frontmatter");
        assert_eq!(
            fm.extra.get("github").and_then(|v| v.as_str()),
            Some("org/repo")
        );
    }

    #[test]
    fn test_parse_frontmatter_missing() {
        let text = "# Just a heading\n\nSome content.";
        assert!(parse_frontmatter(text).is_none());
    }
}

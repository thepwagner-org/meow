use anyhow::{bail, Result};
use fuzzy_matcher::skim::SkimMatcherV2;
use fuzzy_matcher::FuzzyMatcher;
use inquire::ui::{Color, RenderConfig, StyleSheet, Styled};
use inquire::Select;
use is_terminal::IsTerminal;
use tracing::error;

fn render_config() -> RenderConfig<'static> {
    let pink = Color::Rgb {
        r: 0xff,
        g: 0x79,
        b: 0xc6,
    };
    let purple = Color::Rgb {
        r: 0xb3,
        g: 0x79,
        b: 0xed,
    };
    let mut config = RenderConfig::empty();
    config.prompt_prefix = Styled::new("");
    config.highlighted_option_prefix = Styled::new("› ").with_fg(purple);
    config.selected_option = Some(StyleSheet::new().with_fg(purple));
    config.answer = StyleSheet::new().with_fg(pink);
    config
}

/// Pick from items interactively, or return error if not a TTY.
///
/// When stdin is not a TTY (e.g., piped input):
/// - Prints available items to stderr
/// - Returns error suggesting to provide a query
pub fn pick(items: Vec<String>, use_color: bool) -> Result<Option<String>> {
    if items.is_empty() {
        return Ok(None);
    }

    // Check if stdin is a TTY
    if !std::io::stdin().is_terminal() {
        // Print available options to stderr
        error!("Available options:");
        for item in &items {
            error!("  {}", item);
        }
        bail!("Interactive selection not available (not a TTY). Please provide a query argument.");
    }

    let config = if use_color {
        render_config()
    } else {
        RenderConfig::empty()
    };

    let result = Select::new("", items)
        .without_filtering()
        .without_help_message()
        .with_render_config(config)
        .prompt_skippable()?;
    Ok(result)
}

/// Fuzzy-pick from candidates with an optional query.
/// - Exact match bypasses the picker immediately
/// - Single fuzzy match is auto-selected
/// - Multiple matches show the picker
/// - No query shows the full picker
pub fn fuzzy_pick(
    candidates: Vec<String>,
    query: Option<&str>,
    use_color: bool,
) -> Result<Option<String>> {
    match query {
        Some(q) => {
            if candidates.iter().any(|c| c == q) {
                return Ok(Some(q.to_string()));
            }
            let matches = fuzzy_match(&candidates, q);
            match matches.len() {
                1 => Ok(Some(matches[0].clone())),
                0 => pick(candidates, use_color),
                _ => pick(matches, use_color),
            }
        }
        None => pick(candidates, use_color),
    }
}

/// Fuzzy match projects using skim algorithm (like fzf), sorted by score
pub fn fuzzy_match(projects: &[String], query: &str) -> Vec<String> {
    let matcher = SkimMatcherV2::default();
    let mut scored: Vec<_> = projects
        .iter()
        .filter_map(|p| {
            matcher
                .fuzzy_match(p, query)
                .map(|score| (p.clone(), score))
        })
        .collect();
    // Sort by score descending (higher = better match)
    scored.sort_by(|a, b| b.1.cmp(&a.1));
    scored.into_iter().map(|(p, _)| p).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_match_empty() {
        let projects: Vec<String> = vec![];
        assert_eq!(fuzzy_match(&projects, "foo"), Vec::<String>::new());
    }

    #[test]
    fn test_fuzzy_match_no_matches() {
        let projects = vec!["alpha".to_string(), "beta".to_string()];
        assert_eq!(fuzzy_match(&projects, "xyz"), Vec::<String>::new());
    }

    #[test]
    fn test_fuzzy_match_smart_case() {
        // Lowercase query = case insensitive (like fzf)
        let projects = vec!["Alpha".to_string(), "BETA".to_string()];
        assert_eq!(fuzzy_match(&projects, "alpha"), vec!["Alpha"]);
        assert_eq!(fuzzy_match(&projects, "beta"), vec!["BETA"]);
        // Uppercase query = case sensitive
        assert_eq!(fuzzy_match(&projects, "BETA"), vec!["BETA"]);
        assert!(fuzzy_match(&projects, "ALPHA").is_empty()); // No match - "Alpha" != "ALPHA"
    }

    #[test]
    fn test_fuzzy_match_prefix_scores_higher() {
        let projects = vec!["bar-foo".to_string(), "foobar".to_string()];
        let matches = fuzzy_match(&projects, "foo");
        // "foobar" should score higher (prefix match)
        assert_eq!(matches[0], "foobar");
    }

    #[test]
    fn test_fuzzy_match_non_contiguous() {
        // Fuzzy matching should match "nj" against "nix-jail"
        let projects = vec!["nix-jail".to_string(), "meow".to_string()];
        let matches = fuzzy_match(&projects, "nj");
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0], "nix-jail");
    }

    #[test]
    fn test_fuzzy_pick_exact_match_wins() {
        // "meow" should resolve immediately even though "meowser" also fuzzy-matches
        let candidates = vec!["meow".to_string(), "meowser".to_string()];
        let result = fuzzy_pick(candidates, Some("meow"), false).unwrap();
        assert_eq!(result, Some("meow".to_string()));
    }

    #[test]
    fn test_fuzzy_pick_single_fuzzy_match() {
        // Only one fuzzy match → auto-select without picker
        let candidates = vec!["alpha".to_string(), "beta".to_string()];
        let result = fuzzy_pick(candidates, Some("alp"), false).unwrap();
        assert_eq!(result, Some("alpha".to_string()));
    }
}

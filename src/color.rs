//! Color output control for terminal rendering.
//!
//! Supports NO_COLOR environment variable, --color CLI flag, and TTY detection.

use is_terminal::IsTerminal;
use std::io::stdout;
use std::str::FromStr;

/// When to use colored output.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ColorChoice {
    /// Automatically detect based on TTY and NO_COLOR
    Auto,
    /// Always use colors
    Always,
    /// Never use colors
    Never,
}

impl FromStr for ColorChoice {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "auto" => Ok(ColorChoice::Auto),
            "always" => Ok(ColorChoice::Always),
            "never" => Ok(ColorChoice::Never),
            _ => Err(format!(
                "Invalid color value '{}'. Use: auto, always, or never",
                s
            )),
        }
    }
}

/// Determine whether to use colors based on choice, NO_COLOR env var, and TTY.
///
/// Priority (highest to lowest):
/// 1. ColorChoice::Always → force colors
/// 2. ColorChoice::Never → no colors
/// 3. NO_COLOR env var (if set) → no colors
/// 4. ColorChoice::Auto → detect TTY (stdout)
pub fn should_use_color(choice: ColorChoice) -> bool {
    match choice {
        ColorChoice::Always => true,
        ColorChoice::Never => false,
        ColorChoice::Auto => {
            // Check NO_COLOR environment variable
            if std::env::var_os("NO_COLOR").is_some() {
                return false;
            }
            // Check if stdout is a TTY
            stdout().is_terminal()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_color_choice() {
        assert_eq!("auto".parse::<ColorChoice>().unwrap(), ColorChoice::Auto);
        assert_eq!(
            "always".parse::<ColorChoice>().unwrap(),
            ColorChoice::Always
        );
        assert_eq!("never".parse::<ColorChoice>().unwrap(), ColorChoice::Never);
        assert_eq!("AUTO".parse::<ColorChoice>().unwrap(), ColorChoice::Auto);
        assert!("invalid".parse::<ColorChoice>().is_err());
    }

    #[test]
    fn test_should_use_color_always() {
        assert!(should_use_color(ColorChoice::Always));
    }

    #[test]
    fn test_should_use_color_never() {
        assert!(!should_use_color(ColorChoice::Never));
    }

    #[test]
    fn test_should_use_color_auto_respects_no_color() {
        // Note: This test can't fully validate NO_COLOR behavior
        // because it depends on environment state
        std::env::set_var("NO_COLOR", "1");
        assert!(!should_use_color(ColorChoice::Auto));
        std::env::remove_var("NO_COLOR");
    }
}

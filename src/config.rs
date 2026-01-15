use anyhow::{Context, Result};
use serde::Deserialize;
use std::env;
use std::path::PathBuf;

/// Returns the XDG config directory for meow.
/// Uses $XDG_CONFIG_HOME/meow/ or ~/.config/meow/
pub fn config_dir() -> Result<PathBuf> {
    let base = if let Ok(xdg) = env::var("XDG_CONFIG_HOME") {
        PathBuf::from(xdg)
    } else {
        dirs::home_dir()
            .context("Failed to get home directory")?
            .join(".config")
    };
    Ok(base.join("meow"))
}

/// Returns the XDG data directory for meow.
/// Uses $XDG_DATA_HOME/meow/ or ~/.local/share/meow/
pub fn data_dir() -> Result<PathBuf> {
    let base = if let Ok(xdg) = env::var("XDG_DATA_HOME") {
        PathBuf::from(xdg)
    } else {
        dirs::home_dir()
            .context("Failed to get home directory")?
            .join(".local/share")
    };
    Ok(base.join("meow"))
}

/// Expands ~ at the start of a path to the home directory.
fn expand_tilde(path: &str) -> Result<PathBuf> {
    if let Some(rest) = path.strip_prefix("~/") {
        let home = dirs::home_dir().context("Failed to get home directory")?;
        Ok(home.join(rest))
    } else if path == "~" {
        dirs::home_dir().context("Failed to get home directory")
    } else {
        Ok(PathBuf::from(path))
    }
}

#[derive(Debug, Deserialize)]
struct ConfigFile {
    repo_path: Option<String>,
}

#[derive(Debug)]
pub struct Config {
    pub repo_path: PathBuf,
}

impl Default for Config {
    fn default() -> Self {
        let repo_path = dirs::home_dir()
            .map(|h| h.join("src"))
            .unwrap_or_else(|| PathBuf::from("/src"));
        Self { repo_path }
    }
}

impl Config {
    /// Load config from ~/.config/meow/config.yaml (or XDG equivalent).
    /// Falls back to defaults if file doesn't exist.
    pub fn load() -> Result<Self> {
        let config_path = config_dir()?.join("config.yaml");

        if !config_path.exists() {
            return Ok(Self::default());
        }

        let content = std::fs::read_to_string(&config_path)
            .with_context(|| format!("Failed to read {}", config_path.display()))?;

        let file: ConfigFile = serde_yaml::from_str(&content)
            .with_context(|| format!("Failed to parse {}", config_path.display()))?;

        let repo_path = match file.repo_path {
            Some(p) => expand_tilde(&p)?,
            None => Self::default().repo_path,
        };

        Ok(Self { repo_path })
    }
}

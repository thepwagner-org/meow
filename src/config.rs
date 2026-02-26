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

// ── Raw deserialization structs ───────────────────────────────────────────────

#[derive(Debug, Deserialize, Default)]
struct HostConfigFile {
    hostname: String,
    tls_cert: Option<String>,
    tls_key: Option<String>,
}

/// Raw web config as it appears in the YAML file.
/// Supports both the legacy flat format and the new `hosts` array.
///
/// Legacy (single host):
/// ```yaml
/// web:
///   hostname: code.example.net
///   tls_cert: ~/.config/meow/cert.pem
///   tls_key:  ~/.config/meow/key.pem
/// ```
///
/// New (multiple virtual hosts):
/// ```yaml
/// web:
///   http_port: 3080
///   port: 3443
///   hosts:
///     - hostname: localhost
///     - hostname: code.example.net
///       tls_cert: ~/.config/meow/cert.pem
///       tls_key:  ~/.config/meow/key.pem
/// ```
#[derive(Debug, Deserialize, Default)]
struct WebConfigFile {
    // Legacy single-host fields
    hostname: Option<String>,
    tls_cert: Option<String>,
    tls_key: Option<String>,
    // New multi-host fields
    hosts: Option<Vec<HostConfigFile>>,
    bind: Option<String>,
    port: Option<u16>,
    http_port: Option<u16>,
}

#[derive(Debug, Deserialize)]
struct ConfigFile {
    repo_path: Option<String>,
    #[serde(default)]
    web: WebConfigFile,
}

// ── Public config structs ─────────────────────────────────────────────────────

/// Configuration for a single virtual host.
#[derive(Debug, Clone)]
pub struct HostConfig {
    /// Public hostname (e.g. "code.example.net" or "localhost").
    pub hostname: String,
    /// Path to TLS certificate PEM file. If absent, this host is served over HTTP.
    pub tls_cert: Option<PathBuf>,
    /// Path to TLS private key PEM file. If absent, this host is served over HTTP.
    pub tls_key: Option<PathBuf>,
}

impl HostConfig {
    pub fn tls_enabled(&self) -> bool {
        self.tls_cert.is_some() && self.tls_key.is_some()
    }
}

/// Web proxy configuration.
#[derive(Debug, Clone)]
pub struct WebConfig {
    /// Virtual hosts to serve. At least one must be present.
    pub hosts: Vec<HostConfig>,
    /// IP address to bind the server on (e.g. "0.0.0.0" or "127.0.0.1")
    pub bind: String,
    /// HTTPS port (only bound when at least one host has TLS configured).
    pub port: u16,
    /// HTTP port (always bound).
    pub http_port: u16,
}

impl WebConfig {
    /// Returns true if any virtual host has TLS configured.
    pub fn has_tls_hosts(&self) -> bool {
        self.hosts.iter().any(|h| h.tls_enabled())
    }

    /// Returns only the TLS-enabled hosts.
    pub fn tls_hosts(&self) -> Vec<&HostConfig> {
        self.hosts.iter().filter(|h| h.tls_enabled()).collect()
    }

    /// Look up the host config that matches a bare hostname (no port).
    /// Returns `None` if no configured host matches.
    pub fn host_config_for(&self, bare_hostname: &str) -> Option<&HostConfig> {
        self.hosts.iter().find(|h| h.hostname == bare_hostname)
    }
}

impl Default for WebConfig {
    fn default() -> Self {
        Self {
            hosts: vec![HostConfig {
                hostname: "localhost".to_string(),
                tls_cert: None,
                tls_key: None,
            }],
            bind: "0.0.0.0".to_string(),
            port: 3443,
            http_port: 3080,
        }
    }
}

#[derive(Debug)]
pub struct Config {
    pub repo_path: PathBuf,
    pub web: WebConfig,
}

impl Default for Config {
    fn default() -> Self {
        let repo_path = dirs::home_dir()
            .map(|h| h.join("src"))
            .unwrap_or_else(|| PathBuf::from("/src"));
        Self {
            repo_path,
            web: WebConfig::default(),
        }
    }
}

impl Config {
    /// Load config from ~/.config/meow/config.yaml (or XDG equivalent).
    /// Falls back to defaults if file doesn't exist.
    ///
    /// Environment variable overrides:
    /// - `MEOW_REPO_PATH`: Override the repository path (useful for CI)
    pub fn load() -> Result<Self> {
        // Environment variable takes precedence
        if let Ok(repo_path) = env::var("MEOW_REPO_PATH") {
            return Ok(Self {
                repo_path: PathBuf::from(repo_path),
                web: WebConfig::default(),
            });
        }

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

        let web = {
            let f = file.web;
            let defaults = WebConfig::default();

            // Resolve the hosts list. Prefer the new `hosts` array; fall back to
            // legacy flat fields (hostname / tls_cert / tls_key) as a single entry.
            let hosts: Vec<HostConfig> = if let Some(raw_hosts) = f.hosts {
                raw_hosts
                    .into_iter()
                    .map(|h| {
                        let tls_cert = h.tls_cert.as_deref().map(expand_tilde).transpose()?;
                        let tls_key = h.tls_key.as_deref().map(expand_tilde).transpose()?;
                        Ok(HostConfig {
                            hostname: h.hostname,
                            tls_cert,
                            tls_key,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?
            } else {
                // Legacy single-host format
                let hostname = f
                    .hostname
                    .unwrap_or_else(|| defaults.hosts[0].hostname.clone());
                let tls_cert = f.tls_cert.as_deref().map(expand_tilde).transpose()?;
                let tls_key = f.tls_key.as_deref().map(expand_tilde).transpose()?;
                vec![HostConfig {
                    hostname,
                    tls_cert,
                    tls_key,
                }]
            };

            if hosts.is_empty() {
                anyhow::bail!("web.hosts must not be empty");
            }

            WebConfig {
                hosts,
                bind: f.bind.unwrap_or(defaults.bind),
                port: f.port.unwrap_or(defaults.port),
                http_port: f.http_port.unwrap_or(defaults.http_port),
            }
        };

        Ok(Self { repo_path, web })
    }
}

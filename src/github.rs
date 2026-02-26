//! GitHub API interactions via the `gh` CLI.

use serde::Deserialize;
use std::collections::HashMap;
use std::fmt::Write;
use std::process::Command;
use tracing::warn;

/// A single Dependabot alert on a repository.
#[derive(Debug, Clone)]
pub struct DependabotAlert {
    pub package: String,
    pub severity: String,
}

impl DependabotAlert {
    /// Single-character severity abbreviation for display.
    pub fn severity_short(&self) -> &'static str {
        severity_short(&self.severity)
    }

    /// Numeric severity for ordering (lower = more severe).
    pub fn severity_ord(&self) -> u8 {
        severity_ord(&self.severity)
    }
}

fn severity_short(s: &str) -> &'static str {
    match s {
        "CRITICAL" => "C",
        "HIGH" => "H",
        "MODERATE" => "M",
        "LOW" => "L",
        _ => "?",
    }
}

fn severity_ord(s: &str) -> u8 {
    match s {
        "CRITICAL" => 0,
        "HIGH" => 1,
        "MODERATE" => 2,
        "LOW" => 3,
        _ => 4,
    }
}

// -- GraphQL response deserialization --

#[derive(Deserialize)]
struct GraphQLResponse {
    data: Option<HashMap<String, Option<RepoData>>>,
    errors: Option<Vec<GraphQLError>>,
}

#[derive(Deserialize)]
struct GraphQLError {
    message: String,
}

#[derive(Deserialize)]
struct RepoData {
    #[serde(rename = "vulnerabilityAlerts")]
    vulnerability_alerts: Option<AlertConnection>,
}

#[derive(Deserialize)]
struct AlertConnection {
    nodes: Vec<AlertNode>,
}

#[derive(Deserialize)]
struct AlertNode {
    #[serde(rename = "securityVulnerability")]
    security_vulnerability: SecurityVulnerability,
}

#[derive(Deserialize)]
struct SecurityVulnerability {
    severity: String,
    package: PackageInfo,
}

#[derive(Deserialize)]
struct PackageInfo {
    name: String,
}

/// Fetch open Dependabot alerts for multiple repositories in a single GraphQL query.
///
/// Returns `None` if the `gh` CLI is not available. Individual repos that are
/// inaccessible will have empty alert vecs.
pub fn fetch_dependabot_alerts(
    repos: &[(String, String)],
) -> Option<HashMap<(String, String), Vec<DependabotAlert>>> {
    if repos.is_empty() {
        return Some(HashMap::new());
    }

    // Verify gh is available
    if Command::new("gh")
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_err()
    {
        warn!("`gh` CLI not found; skipping dependabot alerts");
        return None;
    }

    let query = build_alerts_query(repos);
    let output = Command::new("gh")
        .args(["api", "graphql", "-f", &format!("query={query}")])
        .output();

    let output = match output {
        Ok(o) if o.status.success() => o,
        Ok(o) => {
            let stderr = String::from_utf8_lossy(&o.stderr);
            warn!("gh graphql query failed: {stderr}");
            return None;
        }
        Err(e) => {
            warn!("failed to run gh: {e}");
            return None;
        }
    };

    parse_alerts_response(&output.stdout, repos)
}

/// Parse a GraphQL response into per-repo alert vecs.
///
/// Returns `None` if the response can't be parsed or has no data.
fn parse_alerts_response(
    body: &[u8],
    repos: &[(String, String)],
) -> Option<HashMap<(String, String), Vec<DependabotAlert>>> {
    let response: GraphQLResponse = match serde_json::from_slice(body) {
        Ok(r) => r,
        Err(e) => {
            warn!("failed to parse gh response: {e}");
            return None;
        }
    };

    if let Some(errors) = &response.errors {
        for err in errors {
            warn!("GraphQL error: {}", err.message);
        }
    }

    let data = response.data?;

    let mut result = HashMap::new();
    for (i, (org, repo)) in repos.iter().enumerate() {
        let key = format!("r{i}");
        let alerts = data
            .get(&key)
            .and_then(|r| r.as_ref())
            .and_then(|r| r.vulnerability_alerts.as_ref())
            .map(|conn| {
                conn.nodes
                    .iter()
                    .map(|node| DependabotAlert {
                        package: node.security_vulnerability.package.name.clone(),
                        severity: node.security_vulnerability.severity.clone(),
                    })
                    .collect()
            })
            .unwrap_or_default();
        let _ = result.insert((org.clone(), repo.clone()), alerts);
    }

    Some(result)
}

/// Build a single GraphQL query with a fragment and per-repo aliases.
///
/// ```graphql
/// fragment alerts on Repository {
///   vulnerabilityAlerts(states: OPEN, first: 100) {
///     nodes { securityVulnerability { severity package { name } } }
///   }
/// }
/// {
///   r0: repository(owner: "org", name: "repo") { ...alerts }
///   r1: repository(owner: "org", name: "repo") { ...alerts }
/// }
/// ```
fn build_alerts_query(repos: &[(String, String)]) -> String {
    let mut q = String::from(concat!(
        "fragment alerts on Repository {",
        " vulnerabilityAlerts(states: OPEN, first: 100) {",
        " nodes { securityVulnerability { severity package { name } } }",
        " }",
        " } {",
    ));
    for (i, (org, repo)) in repos.iter().enumerate() {
        let _ = write!(
            q,
            " r{i}: repository(owner: \"{org}\", name: \"{repo}\") {{ ...alerts }}"
        );
    }
    q.push_str(" }");
    q
}

/// Format alerts for table display.
///
/// Deduplicates by package name (keeping highest severity), sorts by severity
/// then name. Returns `?` if alerts are unavailable, empty string if none.
pub fn format_alerts(alerts: &Option<Vec<DependabotAlert>>) -> String {
    let alerts = match alerts {
        None => return "?".to_string(),
        Some(a) if a.is_empty() => return String::new(),
        Some(a) => a,
    };

    // Deduplicate: keep highest severity per package
    let mut by_pkg: HashMap<&str, &DependabotAlert> = HashMap::new();
    for a in alerts {
        let replace = match by_pkg.get(a.package.as_str()) {
            None => true,
            Some(existing) => a.severity_ord() < existing.severity_ord(),
        };
        if replace {
            let _ = by_pkg.insert(&a.package, a);
        }
    }

    // Sort by severity (critical first), then alphabetically
    let mut entries: Vec<_> = by_pkg.into_values().collect();
    entries.sort_by(|a, b| {
        a.severity_ord()
            .cmp(&b.severity_ord())
            .then(a.package.cmp(&b.package))
    });

    entries
        .iter()
        .map(|a| format!("{}({})", a.package, a.severity_short()))
        .collect::<Vec<_>>()
        .join(" ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_alerts_query_uses_fragment() {
        let repos = vec![
            ("octocat".into(), "hello-world".into()),
            ("rust-lang".into(), "rust".into()),
        ];
        let query = build_alerts_query(&repos);
        assert!(query.starts_with("fragment alerts on Repository"));
        assert!(query
            .contains("r0: repository(owner: \"octocat\", name: \"hello-world\") { ...alerts }"));
        assert!(
            query.contains("r1: repository(owner: \"rust-lang\", name: \"rust\") { ...alerts }")
        );
    }

    #[test]
    fn test_parse_response_with_alerts() {
        let json = serde_json::json!({
            "data": {
                "r0": {
                    "vulnerabilityAlerts": {
                        "nodes": [
                            {
                                "securityVulnerability": {
                                    "severity": "HIGH",
                                    "package": { "name": "lodash" }
                                }
                            },
                            {
                                "securityVulnerability": {
                                    "severity": "CRITICAL",
                                    "package": { "name": "axios" }
                                }
                            }
                        ]
                    }
                }
            }
        });
        let repos = vec![("octocat".into(), "hello-world".into())];
        let result = parse_alerts_response(json.to_string().as_bytes(), &repos);
        let map = result.expect("should parse");
        let alerts = &map[&("octocat".to_string(), "hello-world".to_string())];
        assert_eq!(alerts.len(), 2);
        assert_eq!(alerts[0].package, "lodash");
        assert_eq!(alerts[0].severity, "HIGH");
        assert_eq!(alerts[1].package, "axios");
        assert_eq!(alerts[1].severity, "CRITICAL");
    }

    #[test]
    fn test_parse_response_no_alerts() {
        let json = serde_json::json!({
            "data": {
                "r0": {
                    "vulnerabilityAlerts": { "nodes": [] }
                }
            }
        });
        let repos = vec![("octocat".into(), "hello-world".into())];
        let result = parse_alerts_response(json.to_string().as_bytes(), &repos);
        let map = result.expect("should parse");
        let alerts = &map[&("octocat".to_string(), "hello-world".to_string())];
        assert!(alerts.is_empty());
    }

    #[test]
    fn test_parse_response_inaccessible_repo() {
        // When a repo is null (no access), we get an empty vec
        let json = serde_json::json!({
            "data": { "r0": null },
            "errors": [{ "message": "Resource not accessible" }]
        });
        let repos = vec![("private-org".into(), "secret".into())];
        let result = parse_alerts_response(json.to_string().as_bytes(), &repos);
        let map = result.expect("should parse");
        let alerts = &map[&("private-org".to_string(), "secret".to_string())];
        assert!(alerts.is_empty());
    }

    #[test]
    fn test_parse_response_no_data() {
        let json = serde_json::json!({
            "errors": [{ "message": "Bad credentials" }]
        });
        let repos = vec![("octocat".into(), "hello-world".into())];
        let result = parse_alerts_response(json.to_string().as_bytes(), &repos);
        assert!(result.is_none());
    }

    #[test]
    fn test_parse_response_multiple_repos() {
        let json = serde_json::json!({
            "data": {
                "r0": {
                    "vulnerabilityAlerts": {
                        "nodes": [{
                            "securityVulnerability": {
                                "severity": "LOW",
                                "package": { "name": "minimist" }
                            }
                        }]
                    }
                },
                "r1": {
                    "vulnerabilityAlerts": { "nodes": [] }
                }
            }
        });
        let repos = vec![
            ("org".into(), "repo-a".into()),
            ("org".into(), "repo-b".into()),
        ];
        let result = parse_alerts_response(json.to_string().as_bytes(), &repos);
        let map = result.expect("should parse");
        assert_eq!(map[&("org".to_string(), "repo-a".to_string())].len(), 1);
        assert!(map[&("org".to_string(), "repo-b".to_string())].is_empty());
    }

    #[test]
    fn test_parse_response_invalid_json() {
        let repos = vec![("octocat".into(), "hello-world".into())];
        let result = parse_alerts_response(b"not json", &repos);
        assert!(result.is_none());
    }

    #[test]
    fn test_format_alerts_unavailable() {
        assert_eq!(format_alerts(&None), "?");
    }

    #[test]
    fn test_format_alerts_empty() {
        assert_eq!(format_alerts(&Some(vec![])), "");
    }

    #[test]
    fn test_format_alerts_deduplicates_by_highest_severity() {
        let alerts = vec![
            DependabotAlert {
                package: "lodash".into(),
                severity: "HIGH".into(),
            },
            DependabotAlert {
                package: "lodash".into(),
                severity: "CRITICAL".into(),
            },
            DependabotAlert {
                package: "lodash".into(),
                severity: "LOW".into(),
            },
        ];
        assert_eq!(format_alerts(&Some(alerts)), "lodash(C)");
    }

    #[test]
    fn test_format_alerts_sorted_by_severity_then_name() {
        let alerts = vec![
            DependabotAlert {
                package: "zlib".into(),
                severity: "LOW".into(),
            },
            DependabotAlert {
                package: "axios".into(),
                severity: "CRITICAL".into(),
            },
            DependabotAlert {
                package: "express".into(),
                severity: "HIGH".into(),
            },
            DependabotAlert {
                package: "bcrypt".into(),
                severity: "CRITICAL".into(),
            },
        ];
        assert_eq!(
            format_alerts(&Some(alerts)),
            "axios(C) bcrypt(C) express(H) zlib(L)"
        );
    }
}

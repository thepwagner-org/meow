//! `meow web` - HTTPS reverse proxy for opencode web instances.
//!
//! First invocation starts the proxy in the foreground and spawns an opencode
//! child process for the requested project. Subsequent invocations detect the
//! running proxy via a portfile and POST to its HTTP management API.
//!
//! Traffic is routed by subdomain: `{project}.{hostname}` proxies to that
//! session's opencode backend. The apex domain serves the landing page.
//!
//! Virtual host support: multiple hostnames may be configured. Hosts with TLS
//! certs are served over HTTPS (on `web.port`); hosts without are served over
//! plain HTTP (on `web.http_port`). HTTP requests for a TLS-enabled hostname
//! receive a 301 redirect to the HTTPS equivalent.

/// Enable per-process Basic Auth for opencode backends.
///
/// Disabled until <https://github.com/anomalyco/opencode/pull/9987> merges.
/// When `true`, a random 32-char password is generated for each opencode
/// process and forwarded via `OPENCODE_SERVER_PASSWORD`; the proxy then
/// injects `Authorization: Basic …` on every request it forwards.
/// When `false`, no password is set and no auth headers are sent — safe
/// because opencode only binds to 127.0.0.1 and is not directly reachable.
const OPENCODE_AUTH: bool = false;

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::{bail, Context, Result};
use axum::body::Body;
use axum::extract::ws::{Message as AxMsg, WebSocket, WebSocketUpgrade};
use axum::extract::{FromRequestParts, State};
use axum::http::{HeaderMap, Request, StatusCode, Uri};
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::Router;
use futures_util::{SinkExt, StreamExt};
use http_body_util::BodyExt;
use hyper_util::client::legacy::Client;
use hyper_util::rt::TokioExecutor;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Child;
use tokio::sync::{broadcast, Mutex, RwLock};
use tokio_tungstenite::connect_async;
use tokio_tungstenite::tungstenite::Message as TungMsg;
use tracing::{info, warn};

use crate::commands::{gather_project_info, ProjectInfo};
use crate::config::{data_dir, HostConfig, WebConfig};
use crate::{git, sparse, PROJECTS_DIR};

/// Sanitise a user-supplied worktree name to lowercase kebab-case.
fn to_kebab(s: &str) -> String {
    s.chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() {
                c.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

// ── Portfile ──────────────────────────────────────────────────────────────────

fn portfile_path() -> Result<PathBuf> {
    Ok(data_dir()?.join("web.port"))
}

async fn write_portfile(port: u16) -> Result<()> {
    let path = portfile_path()?;
    if let Some(parent) = path.parent() {
        tokio::fs::create_dir_all(parent)
            .await
            .context("Failed to create data directory")?;
    }
    tokio::fs::write(&path, port.to_string())
        .await
        .context("Failed to write portfile")
}

async fn remove_portfile() {
    if let Ok(path) = portfile_path() {
        let _ = tokio::fs::remove_file(&path).await;
    }
}

/// Read the port of a running proxy, or None if no proxy is running.
pub async fn running_proxy_port() -> Option<u16> {
    let path = portfile_path().ok()?;
    let text = tokio::fs::read_to_string(&path).await.ok()?;
    let port: u16 = text.trim().parse().ok()?;
    // Verify alive
    let url = format!("http://127.0.0.1:{}/api/v1/sessions", port);
    let _resp = reqwest::get(&url).await.ok()?;
    Some(port)
}

// ── Session info ──────────────────────────────────────────────────────────────

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct SessionInfo {
    pub project: String,
    pub port: u16,
    /// Absolute filesystem path of the project directory.
    pub directory: String,
    /// Unix epoch seconds when the session was started.
    pub started_at: u64,
    /// Unix epoch seconds of the last proxied request.
    pub last_request: u64,
    /// Basic Auth password for this opencode instance (not serialised in API responses).
    #[serde(skip)]
    pub password: String,
}

// ── Session events (SSE) ──────────────────────────────────────────────────────

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", content = "session")]
pub enum SessionEvent {
    #[serde(rename = "session:started")]
    Started(SessionInfo),
    #[serde(rename = "session:stopped")]
    Stopped { project: String },
    /// Sentinel emitted after all current sessions have been replayed on connect.
    /// Lets subscribers know the initial snapshot is complete and live events follow.
    #[serde(rename = "session:connected")]
    Connected,
}

// ── Per-project opencode process ──────────────────────────────────────────────

struct OpenCodeProcess {
    port: u16,
    password: String,
    directory: PathBuf,
    started_at: u64,
    last_request: Arc<AtomicU64>,
    _child: Child,
}

fn epoch_secs() -> u64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
}

// ── Shared proxy state ────────────────────────────────────────────────────────

/// A cached asset response: content-type + body bytes.
struct CachedAsset {
    content_type: String,
    bytes: bytes::Bytes,
}

struct ProxyState {
    /// All configured virtual hosts. Used to match incoming `Host` headers to
    /// their host config (which determines scheme, port, TLS, etc.).
    hosts: Vec<HostConfig>,
    /// HTTPS port (only bound when at least one host has TLS configured).
    https_port: u16,
    /// HTTP port (always bound; also used for HTTP→HTTPS redirects).
    http_port: u16,
    /// Repository root (parent of `projects/`).
    repo_root: PathBuf,
    sessions: RwLock<HashMap<String, Arc<Mutex<OpenCodeProcess>>>>,
    /// In-memory cache for /assets/* — content-hashed so safe to keep forever.
    asset_cache: RwLock<HashMap<String, Arc<CachedAsset>>>,
    http_client: Client<hyper_util::client::legacy::connect::HttpConnector, Body>,
    /// Broadcast channel for session lifecycle events (SSE).
    events_tx: broadcast::Sender<SessionEvent>,
}

impl ProxyState {
    fn new(hosts: Vec<HostConfig>, https_port: u16, http_port: u16, repo_root: PathBuf) -> Self {
        let http_client = Client::builder(TokioExecutor::new()).build_http();
        let (events_tx, _) = broadcast::channel(64);
        Self {
            hosts,
            https_port,
            http_port,
            repo_root,
            sessions: RwLock::new(HashMap::new()),
            asset_cache: RwLock::new(HashMap::new()),
            http_client,
            events_tx,
        }
    }

    /// Look up the `HostConfig` matching a `Host` header value (which may
    /// contain a port suffix, e.g. `"code.example.net:3443"`).
    fn host_config_for(&self, host: &str) -> Option<&HostConfig> {
        let bare = host.split(':').next().unwrap_or(host);
        // Match the apex hostname OR any subdomain of it.
        self.hosts
            .iter()
            .find(|h| bare == h.hostname || bare.ends_with(&format!(".{}", h.hostname)))
    }

    /// Extract the project name from a `Host` header value, matching against
    /// all configured virtual hostnames.
    ///
    /// `meow.code.example.net` → `Some("meow")`
    /// `home.code.example.net` → `None` (reserved for landing page)
    /// `code.example.net`      → `None` (apex)
    fn project_from_host(&self, host: &str) -> Option<String> {
        // Strip port suffix if present (e.g. "meow.localhost:3080")
        let host_bare = host.split(':').next().unwrap_or(host);
        // Try each configured virtual host.
        for hc in &self.hosts {
            let suffix = format!(".{}", hc.hostname);
            if let Some(subdomain) = host_bare.strip_suffix(&suffix) {
                // Reject multi-level subdomains, empty strings, and reserved names
                if subdomain.is_empty() || subdomain.contains('.') || subdomain == "home" {
                    return None;
                }
                return Some(subdomain.to_string());
            }
        }
        None
    }

    /// Returns the home URL (landing page) for use in redirects and links,
    /// picking the right scheme and port based on which virtual host the
    /// request arrived on.
    ///
    /// `incoming_host` is the raw `Host` header value (may include port).
    fn home_url_for(&self, incoming_host: &str) -> String {
        let bare = incoming_host.split(':').next().unwrap_or(incoming_host);
        // Find the matching host config to determine scheme.
        let hc = self.host_config_for(bare);
        let tls = hc.map(|h| h.tls_enabled()).unwrap_or(false);
        let hostname = hc.map(|h| h.hostname.as_str()).unwrap_or(bare);
        let port = if tls { self.https_port } else { self.http_port };
        let port_suffix = match (tls, port) {
            (true, 443) | (false, 80) => String::new(),
            (_, p) => format!(":{}", p),
        };
        let scheme = if tls { "https" } else { "http" };
        format!("{}://home.{}{}/", scheme, hostname, port_suffix)
    }

    /// Returns the HTTPS home URL for a specific hostname (used for HTTP→HTTPS
    /// redirects where the target is always the TLS port).
    fn https_home_url_for(&self, hostname: &str) -> String {
        let port_suffix = if self.https_port == 443 {
            String::new()
        } else {
            format!(":{}", self.https_port)
        };
        format!("https://home.{}{}/", hostname, port_suffix)
    }

    async fn add_project(&self, project: &str, project_path: &Path) -> Result<u16> {
        {
            let sessions = self.sessions.read().await;
            if let Some(proc) = sessions.get(project) {
                let p = proc.lock().await;
                return Ok(p.port);
            }
        }
        let (child, port, password) = spawn_opencode(project, project_path).await?;
        info!(project, port, "spawned opencode serve");
        let now = epoch_secs();
        let mut sessions = self.sessions.write().await;
        let _prev = sessions.insert(
            project.to_string(),
            Arc::new(Mutex::new(OpenCodeProcess {
                port,
                password: password.clone(),
                directory: project_path.to_path_buf(),
                started_at: now,
                last_request: Arc::new(AtomicU64::new(now)),
                _child: child,
            })),
        );
        // Notify SSE subscribers about the new session.
        let _ = self.events_tx.send(SessionEvent::Started(SessionInfo {
            project: project.to_string(),
            port,
            password,
            directory: project_path.to_string_lossy().into_owned(),
            started_at: now,
            last_request: now,
        }));
        Ok(port)
    }

    async fn list_sessions(&self) -> Vec<SessionInfo> {
        let sessions = self.sessions.read().await;
        let mut result = Vec::new();
        for (project, proc) in sessions.iter() {
            let p = proc.lock().await;
            result.push(SessionInfo {
                project: project.clone(),
                port: p.port,
                password: p.password.clone(),
                directory: p.directory.to_string_lossy().into_owned(),
                started_at: p.started_at,
                last_request: p.last_request.load(Ordering::Relaxed),
            });
        }
        result
    }

    async fn stop_project(&self, project: &str) -> Result<()> {
        let mut sessions = self.sessions.write().await;
        if sessions.remove(project).is_none() {
            bail!("No active session for project '{}'", project);
        }
        info!(project, "stopped opencode serve");
        let _ = self.events_tx.send(SessionEvent::Stopped {
            project: project.to_string(),
        });
        Ok(())
    }

    async fn get_session(&self, project: &str) -> Option<SessionInfo> {
        let sessions = self.sessions.read().await;
        let proc = sessions.get(project)?;
        let p = proc.lock().await;
        Some(SessionInfo {
            project: project.to_string(),
            port: p.port,
            password: p.password.clone(),
            directory: p.directory.to_string_lossy().into_owned(),
            started_at: p.started_at,
            last_request: p.last_request.load(Ordering::Relaxed),
        })
    }

    /// Get the last_request atomic for a project so the proxy handler can
    /// bump it without holding the session lock.
    async fn get_last_request(&self, project: &str) -> Option<Arc<AtomicU64>> {
        let sessions = self.sessions.read().await;
        let proc = sessions.get(project)?;
        let p = proc.lock().await;
        Some(Arc::clone(&p.last_request))
    }

    /// List ALL focused projects with metadata (both active and inactive).
    async fn list_all_focused_projects(&self) -> Vec<ProjectInfo> {
        let focused = sparse::get_focused_projects(&self.repo_root).unwrap_or_default();
        if focused.is_empty() {
            return Vec::new();
        }
        let repo = match git::find_repo() {
            Ok(r) => r,
            Err(_) => return focused.into_iter().map(ProjectInfo::name_only).collect(),
        };
        let root = self.repo_root.clone();
        gather_project_info(&repo, &root, &focused, &[]).unwrap_or_default()
    }

    /// Serve an /assets/* path from the in-process cache.
    /// On miss, fetches from any live backend and stores the result.
    async fn serve_asset(&self, path: &str) -> Option<Response> {
        // Cache hit
        if let Some(asset) = self.asset_cache.read().await.get(path) {
            let asset = Arc::clone(asset);
            return Some(cached_asset_response(&asset));
        }

        // Fetch from any available backend
        let (port, password) = {
            let sessions = self.sessions.read().await;
            match sessions.values().next() {
                Some(p) => {
                    let proc = p.lock().await;
                    (proc.port, proc.password.clone())
                }
                None => return None,
            }
        };

        let url = format!("http://127.0.0.1:{}{}", port, path);
        let mut builder = axum::http::Request::builder()
            .uri(url.parse::<Uri>().ok()?)
            .version(axum::http::Version::HTTP_11);
        if !password.is_empty() {
            use base64::Engine as _;
            let credentials =
                base64::engine::general_purpose::STANDARD.encode(format!("opencode:{}", password));
            builder = builder.header("authorization", format!("Basic {}", credentials));
        }
        let resp = self
            .http_client
            .request(builder.body(Body::empty()).ok()?)
            .await
            .ok()?;

        if !resp.status().is_success() {
            return None;
        }

        let content_type = resp
            .headers()
            .get(axum::http::header::CONTENT_TYPE)
            .and_then(|v| v.to_str().ok())
            .unwrap_or("application/octet-stream")
            .to_string();

        let body_bytes = resp.into_body().collect().await.ok()?.to_bytes();

        let asset = Arc::new(CachedAsset {
            content_type,
            bytes: body_bytes,
        });
        let _ = self
            .asset_cache
            .write()
            .await
            .insert(path.to_string(), Arc::clone(&asset));
        Some(cached_asset_response(&asset))
    }
}

fn cached_asset_response(asset: &CachedAsset) -> Response {
    (
        StatusCode::OK,
        [
            (
                axum::http::header::CONTENT_TYPE,
                axum::http::HeaderValue::from_str(&asset.content_type).unwrap_or_else(|_| {
                    axum::http::HeaderValue::from_static("application/octet-stream")
                }),
            ),
            (
                axum::http::header::CACHE_CONTROL,
                axum::http::HeaderValue::from_static("public, max-age=31536000, immutable"),
            ),
        ],
        asset.bytes.clone(),
    )
        .into_response()
}

// ── Spawn opencode serve ──────────────────────────────────────────────────────

async fn spawn_opencode(project: &str, project_path: &Path) -> Result<(Child, u16, String)> {
    use tokio::process::Command;

    let password: String = if OPENCODE_AUTH {
        use rand::Rng as _;
        rand::thread_rng()
            .sample_iter(rand::distributions::Alphanumeric)
            .take(32)
            .map(char::from)
            .collect()
    } else {
        String::new()
    };

    // Capture the direnv-augmented environment via null-delimited `env -0`.
    let direnv_output = Command::new("direnv")
        .args(["exec", ".", "env", "-0"])
        .current_dir(project_path)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .output()
        .await
        .context("Failed to run `direnv exec . env -0`")?;

    if !direnv_output.status.success() {
        anyhow::bail!(
            "direnv exec . env -0 failed (exit {}) in {}",
            direnv_output.status,
            project_path.display()
        );
    }

    // Parse null-delimited KEY=VALUE entries, filtering out variables that
    // interfere with opencode subshells (SHELL overrides zsh detection,
    // BASH_ENV causes infinite recursion when direnv spawns bash internally).
    let env_vars: HashMap<String, String> = direnv_output
        .stdout
        .split(|&b| b == 0)
        .filter_map(|entry| {
            if entry.is_empty() {
                return None;
            }
            let s = String::from_utf8_lossy(entry);
            let (key, val) = s.split_once('=')?;
            if matches!(key, "SHELL" | "shell" | "BASH_ENV") {
                return None;
            }
            Some((key.to_owned(), val.to_owned()))
        })
        .collect();

    let mut cmd = Command::new("opencode");
    let _ = cmd
        .args([
            "serve",
            "--port",
            "0",
            "--hostname",
            "127.0.0.1",
            "--print-logs",
        ])
        .env_clear()
        .envs(&env_vars);
    if OPENCODE_AUTH {
        let _ = cmd.env("OPENCODE_SERVER_PASSWORD", &password);
    }
    let mut child = cmd
        .current_dir(project_path)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .context("Failed to spawn opencode serve")?;

    let stdout = child.stdout.take().context("No stdout from opencode")?;
    let stderr = child.stderr.take().context("No stderr from opencode")?;
    let port = read_port_from_output(project, stdout, stderr).await?;
    Ok((child, port, password))
}

async fn read_port_from_output(
    project: &str,
    stdout: impl tokio::io::AsyncRead + Unpin + Send + 'static,
    stderr: impl tokio::io::AsyncRead + Unpin + Send + 'static,
) -> Result<u16> {
    use tokio::sync::oneshot;

    let (tx, rx) = oneshot::channel::<u16>();
    let tx = Arc::new(Mutex::new(Some(tx)));

    let project_out = project.to_owned();
    let tx_out = Arc::clone(&tx);
    let _stdout_task = tokio::spawn(async move {
        let mut lines = BufReader::new(stdout).lines();
        while let Ok(Some(line)) = lines.next_line().await {
            if let Some(port) = extract_port(&line) {
                let mut guard = tx_out.lock().await;
                if let Some(sender) = guard.take() {
                    let _ = sender.send(port);
                }
                // Forward remaining output to tracing
                while let Ok(Some(line)) = lines.next_line().await {
                    info!(project = %project_out, stream = "stdout", "{line}");
                }
                return;
            }
            info!(project = %project_out, stream = "stdout", "{line}");
        }
    });

    let project_err = project.to_owned();
    let tx_err = Arc::clone(&tx);
    let _stderr_task = tokio::spawn(async move {
        let mut lines = BufReader::new(stderr).lines();
        while let Ok(Some(line)) = lines.next_line().await {
            if let Some(port) = extract_port(&line) {
                let mut guard = tx_err.lock().await;
                if let Some(sender) = guard.take() {
                    let _ = sender.send(port);
                }
                // Forward remaining output to tracing
                while let Ok(Some(line)) = lines.next_line().await {
                    info!(project = %project_err, stream = "stderr", "{line}");
                }
                return;
            }
            info!(project = %project_err, stream = "stderr", "{line}");
        }
    });

    tokio::time::timeout(std::time::Duration::from_secs(30), rx)
        .await
        .context("Timed out waiting for opencode serve to start")?
        .context("opencode serve exited before announcing port")
}

fn extract_port(line: &str) -> Option<u16> {
    let re = regex::Regex::new(r":(\d{4,5})(?:[/\s]|$)").ok()?;
    let caps = re.captures(line)?;
    caps.get(1)?.as_str().parse().ok()
}

// ── HTTP handlers ─────────────────────────────────────────────────────────────

type SharedState = Arc<ProxyState>;

// ── Management API (apex domain only) ────────────────────────────────────────

#[derive(Debug, Deserialize)]
struct AddProjectRequest {
    project: String,
    /// Filesystem path. If omitted, derived from repo_root.
    path: Option<String>,
    /// Create a git worktree with this branch name. The session subdomain
    /// becomes the worktree name instead of the project name.
    worktree: Option<String>,
}

#[derive(Debug, Deserialize)]
struct StopProjectRequest {
    project: String,
}

async fn api_sessions(State(state): State<SharedState>) -> impl IntoResponse {
    axum::Json(state.list_sessions().await)
}

async fn api_add(
    State(state): State<SharedState>,
    axum::Json(body): axum::Json<AddProjectRequest>,
) -> Response {
    // Determine the session name (used as subdomain) and filesystem path.
    let (session_name, project_path) = if let Some(wt) = &body.worktree {
        let branch = to_kebab(wt);
        if branch.is_empty() {
            return (
                StatusCode::BAD_REQUEST,
                axum::Json(serde_json::json!({"error": "worktree name is empty after sanitising"})),
            )
                .into_response();
        }
        let repo_root = state.repo_root.clone();
        let project = body.project.clone();
        let branch_clone = branch.clone();
        // create_worktree does sync I/O (git commands) — run off the async executor.
        match tokio::task::spawn_blocking(move || {
            git::create_worktree(&repo_root, &project, Some(&branch_clone))
        })
        .await
        {
            Ok(Ok(path)) => (branch, path),
            Ok(Err(e)) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    axum::Json(serde_json::json!({"error": format!("{:#}", e)})),
                )
                    .into_response()
            }
            Err(e) => {
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    axum::Json(serde_json::json!({"error": format!("task panicked: {}", e)})),
                )
                    .into_response()
            }
        }
    } else {
        let path = match &body.path {
            Some(p) => PathBuf::from(p),
            None => state.repo_root.join(PROJECTS_DIR).join(&body.project),
        };
        (body.project.clone(), path)
    };

    match state.add_project(&session_name, &project_path).await {
        Ok(port) => {
            info!(session = %session_name, project = %body.project, port, "added via API");
            (
                StatusCode::OK,
                axum::Json(serde_json::json!({"port": port, "session": session_name})),
            )
                .into_response()
        }
        Err(e) => (
            StatusCode::INTERNAL_SERVER_ERROR,
            axum::Json(serde_json::json!({"error": format!("{:#}", e)})),
        )
            .into_response(),
    }
}

async fn api_stop(
    State(state): State<SharedState>,
    axum::Json(body): axum::Json<StopProjectRequest>,
) -> Response {
    match state.stop_project(&body.project).await {
        Ok(()) => (StatusCode::OK, axum::Json(serde_json::json!({"ok": true}))).into_response(),
        Err(e) => (
            StatusCode::NOT_FOUND,
            axum::Json(serde_json::json!({"error": format!("{:#}", e)})),
        )
            .into_response(),
    }
}

// ── SSE event stream ──────────────────────────────────────────────────────────

async fn api_events(State(state): State<SharedState>) -> Response {
    // Snapshot current sessions then subscribe — ordering ensures we don't
    // miss events published between the snapshot and the subscription.
    let rx = state.events_tx.subscribe();
    let initial = state.list_sessions().await;

    // Phase 1: emit current sessions as started events.
    // Phase 2: stream live events from the broadcast channel.
    enum Phase {
        Init(Vec<SessionInfo>, broadcast::Receiver<SessionEvent>),
        Live(broadcast::Receiver<SessionEvent>),
    }

    let stream = futures_util::stream::unfold(Phase::Init(initial, rx), |phase| async move {
        match phase {
            Phase::Init(mut sessions, rx) => {
                if let Some(session) = sessions.pop() {
                    let event = SessionEvent::Started(session);
                    let json = serde_json::to_string(&event).ok()?;
                    Some((format!("data: {}\n\n", json), Phase::Init(sessions, rx)))
                } else {
                    // Drain complete — emit sentinel so subscribers know the
                    // initial snapshot is finished, then switch to live events.
                    let json = serde_json::to_string(&SessionEvent::Connected).ok()?;
                    Some((format!("data: {}\n\n", json), Phase::Live(rx)))
                }
            }
            Phase::Live(rx) => recv_next(rx).await,
        }
    })
    .map(Ok::<_, std::convert::Infallible>);

    async fn recv_next(mut rx: broadcast::Receiver<SessionEvent>) -> Option<(String, Phase)> {
        loop {
            match rx.recv().await {
                Ok(event) => {
                    let json = serde_json::to_string(&event).ok()?;
                    return Some((format!("data: {}\n\n", json), Phase::Live(rx)));
                }
                Err(broadcast::error::RecvError::Lagged(n)) => {
                    warn!(n, "SSE subscriber lagged, dropped events");
                    continue;
                }
                Err(broadcast::error::RecvError::Closed) => return None,
            }
        }
    }

    Response::builder()
        .header("content-type", "text/event-stream")
        .header("cache-control", "no-cache")
        .header("x-accel-buffering", "no")
        .body(Body::from_stream(stream))
        .unwrap_or_else(|_| StatusCode::INTERNAL_SERVER_ERROR.into_response())
}

// ── Bootstrap page (.well-known/meow/bootstrap) ───────────────────────────────
//
// Seeds `opencode.global.dat:server` and `opencode.global.dat:model` in
// localStorage so opencode boots directly into the right project.
//
// Flow:
//   1. Browser hits meow.host — subdomain_proxy checks for short-lived cookie
//   2. Cookie absent or stale → 303 to /.well-known/meow/bootstrap
//   3. Server responds with Set-Cookie (Max-Age=120s) + bootstrap HTML
//   4. Bootstrap JS seeds localStorage, then location.replace('/')
//   5. GET / — cookie present → proxy through to opencode
//   6. Cookie expires after 120s; next browser session re-bootstraps

const BOOTSTRAP_PATH: &str = "/.well-known/meow/bootstrap";
/// Base name for the bootstrap cookie.  The actual cookie name includes the
/// listening port so that HTTP (:3080) and HTTPS (:3443) on the same hostname
/// get independent cookies — matching localStorage's per-origin scoping.
const BOOTSTRAP_COOKIE_BASE: &str = "meow-bootstrapped";

/// Bootstrap page served at GET /.well-known/meow/bootstrap.
///
/// The server sets Set-Cookie in the response headers (so the cookie is in the
/// browser jar as soon as the response is received, with no JS timing dependency).
/// The page body seeds opencode's localStorage key, then navigates to /.
fn bootstrap_page(origin: &str, directory: &str) -> String {
    let esc = |s: &str| s.replace('\\', "\\\\").replace('\'', "\\'");
    let origin_js = esc(origin);
    let dir_js = esc(directory);

    format!(
        r#"<!DOCTYPE html>
<html>
<head><meta charset="utf-8"><title>meow</title></head>
<body style="margin:0;background:#131010">

<script>
(function () {{
  var origin = '{origin_js}';
  var dir    = '{dir_js}';
  var key = 'opencode.global.dat:server';
  var state;
  try {{ state = JSON.parse(localStorage.getItem(key) || 'null'); }} catch (e) {{}}
  if (!state || typeof state !== 'object') state = {{ list: [], projects: {{}}, lastProject: {{}} }};
  if (!Array.isArray(state.list)) state.list = [];
  if (!state.projects) state.projects = {{}};
  if (!state.lastProject) state.lastProject = {{}};
  state.projects[origin] = [{{ worktree: dir, expanded: true }}];
  state.lastProject[origin] = dir;
  localStorage.setItem(key, JSON.stringify(state));
  // Seed preferred model so opencode doesn't prompt for one on first load.
  if (!localStorage.getItem('opencode.global.dat:model')) {{
    localStorage.setItem('opencode.global.dat:model', JSON.stringify({{
      user: [{{ modelID: 'claude-sonnet-4-6', providerID: 'anthropic', visibility: 'show' }}],
      recent: [{{ modelID: 'claude-sonnet-4-6', providerID: 'anthropic' }}],
      variant: {{}}
    }}));
  }}
  // Seed layout with panels folded by default.
  if (!localStorage.getItem('opencode.global.dat:layout')) {{
    localStorage.setItem('opencode.global.dat:layout', JSON.stringify({{
      sidebar: {{ opened: false, width: 344, workspaces: {{}}, workspacesDefault: false }},
      terminal: {{ height: 280, opened: false }},
      review: {{ diffStyle: 'split', panelOpened: false }},
      fileTree: {{ opened: false, width: 344, tab: 'changes' }},
      session: {{ width: 600 }},
      mobileSidebar: {{ opened: false }},
      sessionTabs: {{}},
      sessionView: {{}},
      handoff: {{}}
    }}));
  }}
  // Seed settings: disable notifications/sounds, set font.
  if (!localStorage.getItem('settings.v3')) {{
    localStorage.setItem('settings.v3', JSON.stringify({{
      general: {{ autoSave: true, releaseNotes: true, showReasoningSummaries: true }},
      updates: {{ startup: true }},
      appearance: {{ fontSize: 14, font: 'fira-code' }},
      keybinds: {{}},
      permissions: {{ autoApprove: false }},
      notifications: {{ agent: false, permissions: false, errors: false }},
      sounds: {{
        agentEnabled: false, agent: 'staplebops-01',
        permissionsEnabled: false, permissions: 'staplebops-02',
        errorsEnabled: false, errors: 'nope-03'
      }}
    }}));
  }}
  localStorage.setItem('meow:bootstrapped', dir);
  setTimeout(function () {{ location.replace('/'); }}, 100);
}})();
</script>
</body>
</html>"#,
        origin_js = origin_js,
        dir_js = dir_js,
    )
}

/// Percent-encode a string for use as a cookie value (RFC 6265 safe).
fn percent_encode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(b as char)
            }
            _ => out.push_str(&format!("%{:02X}", b)),
        }
    }
    out
}

/// Decode a percent-encoded string.
fn percent_decode(s: &str) -> String {
    let mut out = Vec::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(hex) = std::str::from_utf8(&bytes[i + 1..i + 3]) {
                if let Ok(b) = u8::from_str_radix(hex, 16) {
                    out.push(b);
                    i += 3;
                    continue;
                }
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
}

// ── Landing page (home.{domain}) ─────────────────────────────────────────────

async fn landing_page(state: &ProxyState, incoming_host: &str) -> impl IntoResponse {
    let mut sessions = state.list_sessions().await;
    sessions.sort_by(|a, b| a.project.cmp(&b.project));
    let all_projects = state.list_all_focused_projects().await;
    let active_set: std::collections::HashSet<&str> =
        sessions.iter().map(|s| s.project.as_str()).collect();

    let home_url = state.home_url_for(incoming_host);

    let now = epoch_secs();
    let mut cards = String::new();
    for s in &sessions {
        let url = home_url.replacen("home.", &format!("{}.", s.project), 1);
        cards.push_str(&format!(
            r#"<a class="card" href="{url}" target="_blank" rel="noopener noreferrer">
  <button class="stop-btn" data-project="{project}" title="Stop session">&times;</button>
  <div class="name">{project}</div>
  <div class="dir">{dir}</div>
  <div class="meta">
    <span>started <time data-ts="{started}">{started_rel}</time></span>
    <span>active <time data-ts="{active}">{active_rel}</time></span>
  </div>
</a>"#,
            url = html_escape(&url),
            project = html_escape(&s.project),
            dir = html_escape(&s.directory),
            started = s.started_at,
            started_rel = html_escape(&relative_time(now, s.started_at)),
            active = s.last_request,
            active_rel = html_escape(&relative_time(now, s.last_request)),
        ));
    }

    if sessions.is_empty() {
        cards.push_str(r#"<p class="empty">No active sessions</p>"#);
    }

    let mut picker_rows = String::new();
    for p in &all_projects {
        let created = p
            .created
            .map(|d| d.format("%Y-%m-%d").to_string())
            .unwrap_or_else(|| "-".into());
        let updated = p
            .updated
            .map(|d| d.format("%Y-%m-%d").to_string())
            .unwrap_or_else(|| "-".into());
        let desc = if p.description.is_empty() {
            "-"
        } else {
            &p.description
        };
        let is_active = active_set.contains(p.name.as_str());
        let row_class = if is_active {
            "picker-row hidden"
        } else {
            "picker-row"
        };
        picker_rows.push_str(&format!(
            r#"<tr class="{row_class}" data-project="{name}">
  <td class="col-name">{name}</td>
  <td class="col-desc">{desc}</td>
  <td class="col-date">{created}</td>
  <td class="col-date">{updated}</td>
</tr>"#,
            row_class = row_class,
            name = html_escape(&p.name),
            desc = html_escape(desc),
            created = html_escape(&created),
            updated = html_escape(&updated),
        ));
    }

    let picker_section = if all_projects.is_empty() {
        String::new()
    } else {
        format!(
            r#"<h2>projects</h2>
  <div class="inputs">
    <input id="search" type="text" placeholder="filter..." autocomplete="off" spellcheck="false">
    <div class="wt-wrap">
      <input id="worktree" type="text" placeholder="worktree" autocomplete="off" spellcheck="false">
      <button class="dice-btn" type="button" title="Random name">&#9858;</button>
    </div>
  </div>
  <table class="picker">
    <thead><tr>
      <th class="col-name">project</th>
      <th class="col-desc">description</th>
      <th class="col-date">created</th>
      <th class="col-date">updated</th>
    </tr></thead>
    <tbody>{picker_rows}</tbody>
  </table>"#,
        )
    };

    // The management API lives on the home subdomain itself.
    let api_base = home_url.trim_end_matches('/').to_string();

    let html = format!(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">

<title>meow web</title>
<style>
  * {{ margin: 0; padding: 0; box-sizing: border-box; }}
  body {{
    font-family: 'SF Mono', 'Cascadia Code', 'Fira Code', monospace;
    background: #131010;
    color: #c8c0c0;
    min-height: 100vh;
    padding: 3rem 1.5rem;
  }}
  .container {{ max-width: 1100px; margin: 0 auto; }}
  h1 {{
    font-size: 1.1rem;
    font-weight: 500;
    color: #7c6af7;
    margin-bottom: 1.5rem;
    letter-spacing: 0.05em;
  }}
  h2 {{
    font-size: 0.8rem;
    font-weight: 400;
    color: #6a6060;
    text-transform: uppercase;
    letter-spacing: 0.1em;
    margin: 2.5rem 0 0.75rem;
  }}
  .sessions {{ display: flex; flex-direction: column; gap: 0.75rem; }}
  .card {{
    display: block;
    position: relative;
    background: #1e1a1a;
    border: 1px solid #2a2525;
    border-radius: 6px;
    padding: 1rem 1.25rem;
    text-decoration: none;
    color: inherit;
    transition: border-color 0.15s, background 0.15s;
  }}
  .card:hover {{
    border-color: #7c6af7;
    background: #231f1f;
  }}
  .stop-btn {{
    position: absolute;
    top: 0.6rem;
    right: 0.6rem;
    background: none;
    border: none;
    color: #4a4040;
    font-size: 1.1rem;
    line-height: 1;
    cursor: pointer;
    padding: 0.15rem 0.35rem;
    border-radius: 3px;
    transition: color 0.15s, background 0.15s;
  }}
  .stop-btn:hover {{
    color: #e05050;
    background: rgba(224, 80, 80, 0.1);
  }}
  .name {{
    font-size: 1rem;
    font-weight: 600;
    color: #e8e0e0;
    margin-bottom: 0.35rem;
  }}
  .dir {{
    font-size: 0.75rem;
    color: #6a6060;
    margin-bottom: 0.6rem;
  }}
  .meta {{
    display: flex;
    gap: 1.5rem;
    font-size: 0.75rem;
    color: #8a7f7f;
  }}
  .empty {{
    color: #6a6060;
    font-size: 0.85rem;
    padding: 2rem 0;
  }}
  .inputs {{
    display: flex;
    gap: 0.75rem;
    margin-bottom: 0.75rem;
  }}
  .inputs input {{
    font-family: inherit;
    font-size: 0.85rem;
    background: #1e1a1a;
    color: #e8e0e0;
    border: 1px solid #2a2525;
    border-radius: 4px;
    padding: 0.5rem 0.75rem;
    outline: none;
    transition: border-color 0.15s;
  }}
  .inputs input:focus {{
    border-color: #7c6af7;
  }}
  .inputs input::placeholder {{
    color: #4a4040;
  }}
  #search {{
    flex: 1;
  }}
  .wt-wrap {{
    display: flex;
    align-items: stretch;
  }}
  #worktree {{
    border-radius: 4px 0 0 4px;
    width: 10rem;
  }}
  .dice-btn {{
    font-family: inherit;
    font-size: 0.85rem;
    background: #1e1a1a;
    color: #6a6060;
    border: 1px solid #2a2525;
    border-left: none;
    border-radius: 0 4px 4px 0;
    padding: 0 0.55rem;
    cursor: pointer;
    transition: color 0.15s, background 0.15s;
  }}
  .dice-btn:hover {{
    color: #7c6af7;
    background: #231f1f;
  }}
  .picker {{
    width: 100%;
    border-collapse: collapse;
    font-size: 0.8rem;
  }}
  .picker thead th {{
    text-align: left;
    color: #6a6060;
    font-weight: 400;
    padding: 0.3rem 0.75rem;
    border-bottom: 1px solid #2a2525;
  }}
  .picker-row {{
    cursor: pointer;
    transition: background 0.15s;
  }}
  .picker-row:hover {{
    background: #1e1a1a;
  }}
  .picker-row.loading {{
    opacity: 0.4;
    pointer-events: none;
  }}
  .picker-row.hidden {{
    display: none;
  }}
  .picker-row td {{
    padding: 0.45rem 0.75rem;
    border-bottom: 1px solid #1e1a1a;
  }}
  .col-name {{
    color: #e8e0e0;
    white-space: nowrap;
  }}
  .col-desc {{
    color: #8a7f7f;
  }}
  .col-date {{
    color: #6a6060;
    white-space: nowrap;
    text-align: right;
  }}
  .picker thead .col-date {{
    text-align: right;
  }}
</style>
</head>
<body>
<div class="container">
  <h1>meow web</h1>
  <div class="sessions">{cards}</div>
  {picker_section}
</div>
<script>
(function () {{
  var apiBase = '{api_base}';

  // ── Random name generator ────────────────────────────────
  var adj = [
    'sleepy','fuzzy','curious','sneaky','lazy','fluffy','grumpy','clever',
    'bouncy','dizzy','frisky','jolly','nimble','plucky','quirky','sassy',
    'witty','zany','cosmic','mystic','silent','swift','bold','calm',
    'daring','epic','fierce','gentle','hasty','icy','keen','lofty',
    'mellow','noble','odd','proud','quick','rowdy','sly','tender',
    'vast','warm','young','agile','brave','crisp','dusty','eager',
    'fancy','giddy','happy','itchy','jazzy','kinky','lucky','mighty',
    'nifty','oaky','peppy','risky','snug','tidy','ultra','vivid'
  ];
  var noun = [
    'whiskers','mittens','biscuit','noodle','pickle','waffle','muffin','nugget',
    'pebble','sprout','acorn','badger','chipmunk','donkey','otter','puffin',
    'quokka','raven','stoat','turtle','walrus','ferret','iguana','jackal',
    'koala','lemur','moose','newt','ocelot','parrot','quail','rabbit',
    'salmon','toucan','urchin','viper','wombat','yak','zebra','alpaca',
    'bobcat','condor','dingo','falcon','gecko','heron','impala','jaguar',
    'katydid','lobster','mantis','osprey','pelican','raccoon','squid','tapir'
  ];
  function randomName() {{
    return adj[Math.random()*adj.length|0]+'-'+noun[Math.random()*noun.length|0];
  }}

  // ── Kebab-case helper ────────────────────────────────────
  function toKebab(s) {{
    return s.toLowerCase().replace(/[^a-z0-9]+/g,'-').replace(/^-|-$/g,'');
  }}

  // ── Relative time (mirrors server-side relative_time) ───
  function relativeTime(nowSecs, tsSecs) {{
    var d = Math.max(0, nowSecs - tsSecs);
    if (d <= 4) return 'just now';
    if (d < 60) return d + 's ago';
    if (d < 3600) return Math.floor(d / 60) + 'm ago';
    if (d < 86400) {{
      var h = Math.floor(d / 3600);
      var m = Math.floor((d % 3600) / 60);
      return m === 0 ? h + 'h ago' : h + 'h ' + m + 'm ago';
    }}
    return Math.floor(d / 86400) + 'd ago';
  }}

  // ── HTML escape ──────────────────────────────────────────
  function esc(s) {{
    var el = document.createElement('span');
    el.textContent = s;
    return el.innerHTML;
  }}

  // ── Build session cards HTML from JSON array ─────────────
  function buildCards(sessions) {{
    if (!sessions.length) return '<p class="empty">No active sessions</p>';
    var now = Math.floor(Date.now() / 1000);
    return sessions.map(function (s) {{
      var url = apiBase.replace('home.', s.project + '.') + '/';
      return '<a class="card" href="' + esc(url) + '" target="_blank" rel="noopener noreferrer">'
        + '<button class="stop-btn" data-project="' + esc(s.project) + '" title="Stop session">&times;</button>'
        + '<div class="name">' + esc(s.project) + '</div>'
        + '<div class="dir">' + esc(s.directory) + '</div>'
        + '<div class="meta">'
        + '<span>started <time data-ts="' + s.started_at + '">' + esc(relativeTime(now, s.started_at)) + '</time></span>'
        + '<span>active <time data-ts="' + s.last_request + '">' + esc(relativeTime(now, s.last_request)) + '</time></span>'
        + '</div></a>';
    }}).join('');
  }}

  // ── Live refresh state ───────────────────────────────────
  var lastRefresh = Date.now();
  var dirty = false;
  var refreshing = false;
  var activeProjects = {{}};
  document.querySelectorAll('.stop-btn').forEach(function (b) {{ activeProjects[b.dataset.project] = true; }});
  var STALE_MS = 60000;

  function refresh() {{
    if (refreshing) return;
    refreshing = true;
    fetch(apiBase + '/api/v1/sessions')
      .then(function (r) {{ return r.json(); }})
      .then(function (sessions) {{
        sessions.sort(function (a, b) {{ return a.project < b.project ? -1 : a.project > b.project ? 1 : 0; }});
        // Rebuild session cards
        var container = document.querySelector('.sessions');
        if (container) container.innerHTML = buildCards(sessions);
        // Track active projects for filterRows
        activeProjects = {{}};
        sessions.forEach(function (s) {{ activeProjects[s.project] = true; }});
        // Re-apply combined filter (active + search)
        filterRows();
        lastRefresh = Date.now();
        dirty = false;
      }})
      .catch(function () {{}})
      .finally(function () {{ refreshing = false; }});
  }}

  // ── SSE subscription ─────────────────────────────────────
  var es = new EventSource(apiBase + '/api/v1/events');
  es.onmessage = function () {{
    if (document.hidden) {{
      dirty = true;
    }} else {{
      refresh();
    }}
  }};

  // ── Visibility change ────────────────────────────────────
  document.addEventListener('visibilitychange', function () {{
    if (document.hidden) return;
    if (dirty || Date.now() - lastRefresh > STALE_MS) {{
      refresh();
    }}
  }});

  // ── Inputs ───────────────────────────────────────────────
  var searchEl = document.getElementById('search');
  var wtEl = document.getElementById('worktree');
  var diceBtn = document.querySelector('.dice-btn');

  // Fuzzy filter
  function fuzzy(query, text) {{
    var qi = 0;
    for (var ti = 0; ti < text.length && qi < query.length; ti++) {{
      if (text[ti] === query[qi]) qi++;
    }}
    return qi === query.length;
  }}
  function filterRows() {{
    var q = (searchEl ? searchEl.value : '').toLowerCase();
    document.querySelectorAll('.picker-row').forEach(function (r) {{
      var hide = activeProjects[r.dataset.project]
        || (q.length > 0 && !fuzzy(q, r.dataset.project.toLowerCase()));
      r.classList.toggle('hidden', hide);
    }});
  }}
  if (searchEl) {{
    searchEl.addEventListener('input', filterRows);
    searchEl.addEventListener('keydown', function (e) {{
      if (e.key !== 'Enter') return;
      var visible = [];
      document.querySelectorAll('.picker-row').forEach(function (r) {{
        if (!r.classList.contains('hidden')) visible.push(r);
      }});
      if (visible.length === 1) visible[0].click();
    }});
  }}

  // Worktree kebab-case
  if (wtEl) {{
    wtEl.addEventListener('input', function () {{
      var s = wtEl.selectionStart;
      wtEl.value = toKebab(wtEl.value);
      wtEl.selectionStart = wtEl.selectionEnd = Math.min(s, wtEl.value.length);
    }});
  }}

  // Dice button
  if (diceBtn) {{
    diceBtn.addEventListener('click', function () {{
      if (wtEl) wtEl.value = randomName();
    }});
  }}

  // ── Stop buttons (event delegation on .sessions) ────────
  document.querySelector('.sessions').addEventListener('click', function (e) {{
    var btn = e.target.closest('.stop-btn');
    if (!btn) return;
    e.preventDefault();
    e.stopPropagation();
    var project = btn.dataset.project;
    var card = btn.closest('.card');
    if (card) card.style.opacity = '0.4';
    fetch(apiBase + '/api/v1/stop', {{
      method: 'POST',
      headers: {{ 'Content-Type': 'application/json' }},
      body: JSON.stringify({{ project: project }})
    }}).then(function (resp) {{
      if (!resp.ok) throw new Error('failed');
      refresh();
    }}).catch(function () {{
      if (card) card.style.opacity = '';
    }});
  }});

  // ── Project picker rows (event delegation on table) ─────
  var pickerTable = document.querySelector('.picker');
  if (pickerTable) {{
    pickerTable.addEventListener('click', function (e) {{
      var row = e.target.closest('.picker-row');
      if (!row || row.classList.contains('hidden') || row.classList.contains('loading')) return;
      var project = row.dataset.project;
      var wt = wtEl ? wtEl.value.trim() : '';
      var payload = {{ project: project }};
      if (wt) payload.worktree = wt;
      row.classList.add('loading');
      fetch(apiBase + '/api/v1/add', {{
        method: 'POST',
        headers: {{ 'Content-Type': 'application/json' }},
        body: JSON.stringify(payload)
      }}).then(function (resp) {{
        if (!resp.ok) return resp.json().then(function(b){{ throw new Error(b.error||'failed'); }});
        return resp.json();
      }}).then(function (data) {{
        var session = data.session || project;
        var url = apiBase.replace('home.', session + '.') + '/';
        window.open(url, '_blank');
        refresh();
      }}).catch(function () {{
        row.classList.remove('loading');
      }});
    }});
  }}
}})();
</script>
</body>
</html>"#,
        cards = cards,
        picker_section = picker_section,
        api_base = html_escape(&api_base),
    );

    (
        StatusCode::OK,
        [
            ("content-type", "text/html; charset=utf-8"),
            ("cache-control", "no-store"),
        ],
        html,
    )
}

/// Format a duration between `now` and `ts` (both epoch seconds) as a
/// human-readable relative string like "3m ago" or "just now".
fn relative_time(now: u64, ts: u64) -> String {
    let delta = now.saturating_sub(ts);
    match delta {
        0..=4 => "just now".to_string(),
        5..=59 => format!("{}s ago", delta),
        60..=3599 => format!("{}m ago", delta / 60),
        3600..=86399 => {
            let h = delta / 3600;
            let m = (delta % 3600) / 60;
            if m == 0 {
                format!("{}h ago", h)
            } else {
                format!("{}h {}m ago", h, m)
            }
        }
        _ => {
            let d = delta / 86400;
            format!("{}d ago", d)
        }
    }
}

// ── Via-HTTPS extension ───────────────────────────────────────────────────────
//
// Injected by the HTTPS listener so `subdomain_proxy` knows whether the
// connection arrived over TLS, without needing to inspect the port.

#[derive(Clone, Copy)]
struct ViaHttps(bool);

// ── Subdomain proxy (catch-all) ───────────────────────────────────────────────

async fn subdomain_proxy(State(state): State<SharedState>, req: Request<Body>) -> Response {
    // Extract the effective host. Prefer the `Host` header (HTTP/1.1); fall back
    // to the URI authority component (HTTP/2 uses `:authority` instead of `Host`,
    // which axum surfaces via req.uri().host()).
    let host_header = req
        .headers()
        .get("host")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");
    let host: &str = if host_header.is_empty() {
        req.uri().host().unwrap_or("")
    } else {
        host_header
    };

    // Determine whether this request arrived over TLS.
    let via_https = req
        .extensions()
        .get::<ViaHttps>()
        .map(|v| v.0)
        .unwrap_or(false);

    let host_bare = host.split(':').next().unwrap_or(host);

    // HTTP→HTTPS redirect: if a TLS-enabled host is accessed over plain HTTP,
    // redirect permanently to the HTTPS equivalent.
    if !via_https {
        if let Some(hc) = state.host_config_for(host_bare) {
            if hc.tls_enabled() {
                let https_url = state.https_home_url_for(&hc.hostname);
                // Replace `home.` prefix with the actual path/subdomain from
                // the request. For project subdomains, rewrite to that project.
                let target = if let Some(project) = state.project_from_host(host) {
                    https_url.replacen("home.", &format!("{}.", project), 1)
                } else {
                    // Apex or home → redirect to HTTPS home
                    https_url
                };
                return axum::response::Redirect::permanent(&target).into_response();
            }
        }
    }

    // Check if this is the home subdomain → serve landing page.
    // Match against all configured virtual hostnames.
    let is_home = state
        .hosts
        .iter()
        .any(|hc| host_bare == format!("home.{}", hc.hostname));
    if is_home {
        return landing_page(&state, host).await.into_response();
    }

    let project = match state.project_from_host(host) {
        Some(p) => p,
        None => {
            // Apex or unrecognised host → redirect to home
            return axum::response::Redirect::to(&state.home_url_for(host)).into_response();
        }
    };

    let path = req.uri().path();

    // Serve /assets/* from the in-process cache — shared across all subdomains,
    // no backend round-trip after the first fetch.
    if path.starts_with("/assets/") {
        return match state.serve_asset(path).await {
            Some(r) => r,
            None => (StatusCode::NOT_FOUND, format!("Asset not found: {}", path)).into_response(),
        };
    }

    let session = match state.get_session(&project).await {
        Some(s) => s,
        None => {
            return (
                StatusCode::NOT_FOUND,
                format!("No active session for project '{}'", project),
            )
                .into_response();
        }
    };
    let port = session.port;
    let password = session.password;
    let directory = session.directory;

    // Bootstrap check: if the cookie is absent or doesn't match the current
    // directory, redirect to the bootstrap page which seeds localStorage and
    // sets the cookie before bouncing back. Skip for the bootstrap page itself
    // and for non-navigation subresource fetches that the browser issues
    // automatically (these can't run JS or set cookies usefully).
    let skip_bootstrap = path == BOOTSTRAP_PATH
        || path == "/site.webmanifest"
        || path == "/favicon.ico"
        || path == "/robots.txt";
    if !skip_bootstrap {
        let bound_port = if via_https {
            state.https_port
        } else {
            state.http_port
        };
        let cookie_name = format!("{}-{}", BOOTSTRAP_COOKIE_BASE, bound_port);
        let cookie_header = req
            .headers()
            .get("cookie")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("");
        let bootstrapped_dir = cookie_header.split(';').find_map(|pair| {
            let pair = pair.trim();
            pair.strip_prefix(cookie_name.as_str())
                .and_then(|rest| rest.strip_prefix('='))
                .map(|val| percent_decode(val.trim()))
        });
        if bootstrapped_dir.as_deref() != Some(&directory) {
            info!(
                project,
                path,
                cookie_value = ?bootstrapped_dir,
                expected = %directory,
                "bootstrap redirect — cookie missing or stale"
            );
            // Cache-bust: append a timestamp so the browser never serves a
            // stale cached bootstrap page (which wouldn't process Set-Cookie).
            let bust = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs();
            let location = format!("{}?t={}", BOOTSTRAP_PATH, bust);
            return (
                StatusCode::SEE_OTHER,
                [
                    ("location", location.as_str()),
                    ("cache-control", "no-store"),
                ],
            )
                .into_response();
        }
    }

    // Serve the bootstrap page itself.
    if path == BOOTSTRAP_PATH {
        // Determine scheme and port from which virtual host this is.
        let hc = state.host_config_for(host_bare);
        let tls = hc.map(|h| h.tls_enabled()).unwrap_or(false);
        let hostname = hc.map(|h| h.hostname.as_str()).unwrap_or(host_bare);
        let scheme = if tls { "https" } else { "http" };
        let bound_port = if tls {
            state.https_port
        } else {
            state.http_port
        };
        let port_suffix = match (tls, bound_port) {
            (true, 443) | (false, 80) => String::new(),
            (_, p) => format!(":{}", p),
        };
        // Build origin from the bare hostname (without port) + port_suffix to
        // avoid doubling the port when `host` already contains the port.
        let origin = format!("{}://{}.{}{}", scheme, project, hostname, port_suffix);
        let cookie_val = percent_encode(&directory);
        // Long-lived cookie (1 day). Only purpose is preventing the redirect
        // loop during bootstrap→/ navigation.  The cookie name includes the
        // port so HTTP and HTTPS origins stay independent (cookies ignore port
        // but localStorage does not).
        let secure_flag = if tls { "; Secure" } else { "" };
        let cookie_name = format!("{}-{}", BOOTSTRAP_COOKIE_BASE, bound_port);
        let cookie_header = format!(
            "{}={}; Path=/; Max-Age=86400; SameSite=Lax{}",
            cookie_name, cookie_val, secure_flag
        );
        let html = bootstrap_page(&origin, &directory);
        info!(
            project,
            %origin,
            %directory,
            cookie_val = %cookie_val,
            "serving bootstrap page"
        );
        return (
            StatusCode::OK,
            [
                ("content-type", "text/html; charset=utf-8"),
                ("cache-control", "no-store"),
                ("set-cookie", &cookie_header),
            ],
            html,
        )
            .into_response();
    }

    // Bump last-active timestamp (lock-free).
    if let Some(ts) = state.get_last_request(&project).await {
        ts.store(epoch_secs(), Ordering::Relaxed);
    }

    // WebSocket upgrade
    let is_ws = req
        .headers()
        .get("upgrade")
        .and_then(|v| v.to_str().ok())
        .map(|v| v.eq_ignore_ascii_case("websocket"))
        .unwrap_or(false);

    if is_ws {
        let path_with_query = req
            .uri()
            .path_and_query()
            .map(|pq| pq.as_str().to_string())
            .unwrap_or_else(|| "/".to_string());

        let (mut parts, body) = req.into_parts();
        drop(body);
        let ws = match WebSocketUpgrade::from_request_parts(&mut parts, &()).await {
            Ok(ws) => ws,
            Err(_) => return StatusCode::BAD_REQUEST.into_response(),
        };
        return ws.on_upgrade(move |socket| async move {
            if let Err(e) = proxy_websocket(socket, port, &path_with_query, &password).await {
                warn!("WebSocket proxy error: {:#}", e);
            }
        });
    }

    forward_request(state, req, port, &password)
        .await
        .unwrap_or_else(|e| {
            warn!("Proxy error: {:#}", e);
            StatusCode::BAD_GATEWAY.into_response()
        })
}

async fn forward_request(
    state: SharedState,
    mut req: Request<Body>,
    port: u16,
    password: &str,
) -> Result<Response> {
    let path_and_query = req
        .uri()
        .path_and_query()
        .map(|pq| pq.as_str().to_string())
        .unwrap_or_else(|| "/".to_string());
    let uri_str = format!("http://127.0.0.1:{}{}", port, path_and_query);
    *req.uri_mut() = uri_str.parse::<Uri>()?;
    // Downgrade to HTTP/1.1 — opencode serve only speaks HTTP/1.1, and the
    // incoming request may be HTTP/2 when clients connect over TLS.
    *req.version_mut() = axum::http::Version::HTTP_11;

    strip_hop_by_hop(req.headers_mut());

    if !password.is_empty() {
        use base64::Engine as _;
        let credentials =
            base64::engine::general_purpose::STANDARD.encode(format!("opencode:{}", password));
        let _ = req.headers_mut().insert(
            axum::http::header::AUTHORIZATION,
            format!("Basic {}", credentials).parse()?,
        );
    }

    let resp = state
        .http_client
        .request(req)
        .await
        .context("Backend request failed")?;

    let (parts, body) = resp.into_parts();
    let body = Body::new(body.map_err(axum::Error::new));
    Ok(Response::from_parts(parts, body))
}

async fn proxy_websocket(
    client_socket: WebSocket,
    port: u16,
    path_with_query: &str,
    password: &str,
) -> Result<()> {
    use tokio_tungstenite::tungstenite::client::IntoClientRequest as _;

    let backend_url = format!("ws://127.0.0.1:{}{}", port, path_with_query);

    let mut ws_req = backend_url
        .clone()
        .into_client_request()
        .context("Failed to build WebSocket request")?;
    if !password.is_empty() {
        use base64::Engine as _;
        let credentials =
            base64::engine::general_purpose::STANDARD.encode(format!("opencode:{}", password));
        let _ = ws_req
            .headers_mut()
            .insert("authorization", format!("Basic {}", credentials).parse()?);
    }

    let (backend_ws, _) = connect_async(ws_req)
        .await
        .with_context(|| format!("Failed to connect to backend WebSocket at {}", backend_url))?;

    let (mut backend_sink, mut backend_stream) = backend_ws.split();
    let (mut client_sink, mut client_stream) = client_socket.split();

    let c2b = tokio::spawn(async move {
        while let Some(Ok(msg)) = client_stream.next().await {
            let tung_msg = match msg {
                AxMsg::Text(t) => TungMsg::text(t.as_str()),
                AxMsg::Binary(b) => TungMsg::binary(b),
                AxMsg::Ping(p) => TungMsg::Ping(p),
                AxMsg::Pong(p) => TungMsg::Pong(p),
                AxMsg::Close(_) => break,
            };
            if backend_sink.send(tung_msg).await.is_err() {
                break;
            }
        }
    });

    let b2c = tokio::spawn(async move {
        while let Some(Ok(msg)) = backend_stream.next().await {
            let ax_msg = match msg {
                TungMsg::Text(t) => AxMsg::Text(t.as_str().into()),
                TungMsg::Binary(b) => AxMsg::Binary(b),
                TungMsg::Ping(p) => AxMsg::Ping(p),
                TungMsg::Pong(p) => AxMsg::Pong(p),
                TungMsg::Close(_) | TungMsg::Frame(_) => break,
            };
            if client_sink.send(ax_msg).await.is_err() {
                break;
            }
        }
    });

    tokio::select! {
        _ = c2b => {}
        _ = b2c => {}
    }

    Ok(())
}

fn strip_hop_by_hop(headers: &mut HeaderMap) {
    for name in &[
        "connection",
        "keep-alive",
        "proxy-authenticate",
        "proxy-authorization",
        "te",
        "trailers",
        "transfer-encoding",
        "upgrade",
    ] {
        let _ = headers.remove(*name);
    }
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

// ── TLS: SNI-based multi-cert config ─────────────────────────────────────────

/// Certificate resolver that matches incoming SNI names against configured
/// virtual-host domains using suffix matching.  For a host configured as
/// `example.com`, the cert is served for `example.com` itself *and* any
/// subdomain like `home.example.com` or `myproject.example.com`.
///
/// This replaces `rustls::server::ResolvesServerCertUsingSni` which only does
/// exact hashmap lookup and therefore cannot serve a wildcard cert
/// (`*.example.com`) for arbitrary subdomain SNI names.
#[derive(Debug)]
struct WildcardCertResolver {
    /// Mapping from lowercase domain suffix → cert.  When resolving, we check
    /// if the SNI name equals or is a subdomain of each key.
    entries: Vec<(String, Arc<rustls::sign::CertifiedKey>)>,
}

impl WildcardCertResolver {
    fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    fn add(&mut self, domain: String, key: Arc<rustls::sign::CertifiedKey>) {
        self.entries.push((domain.to_lowercase(), key));
    }
}

impl rustls::server::ResolvesServerCert for WildcardCertResolver {
    fn resolve(
        &self,
        client_hello: rustls::server::ClientHello<'_>,
    ) -> Option<Arc<rustls::sign::CertifiedKey>> {
        let name = client_hello.server_name()?;
        let name_lower = name.to_lowercase();
        self.entries
            .iter()
            .find(|(domain, _)| {
                name_lower == *domain || name_lower.ends_with(&format!(".{domain}"))
            })
            .map(|(_, ck)| ck.clone())
    }
}

/// Build a `rustls::ServerConfig` with SNI-based certificate resolution so
/// each TLS virtual host can have its own cert/key pair.
async fn build_tls_config(tls_hosts: &[&HostConfig]) -> Result<rustls::ServerConfig> {
    use rustls::pki_types::{CertificateDer, PrivateKeyDer};
    use rustls::sign::CertifiedKey;
    use rustls::ServerConfig;
    use rustls_pemfile::{certs, private_key};
    use std::io::BufReader;
    use std::sync::Arc as StdArc;

    let mut resolver = WildcardCertResolver::new();

    for hc in tls_hosts {
        let cert_path = hc
            .tls_cert
            .as_ref()
            .context("tls_cert missing for TLS host")?;
        let key_path = hc
            .tls_key
            .as_ref()
            .context("tls_key missing for TLS host")?;

        let cert_bytes = tokio::fs::read(cert_path)
            .await
            .with_context(|| format!("Failed to read TLS cert: {}", cert_path.display()))?;
        let key_bytes = tokio::fs::read(key_path)
            .await
            .with_context(|| format!("Failed to read TLS key: {}", key_path.display()))?;

        let certs: Vec<CertificateDer<'static>> = certs(&mut BufReader::new(cert_bytes.as_slice()))
            .collect::<std::io::Result<Vec<_>>>()
            .with_context(|| format!("Failed to parse TLS cert: {}", cert_path.display()))?;

        let key: PrivateKeyDer<'static> = private_key(&mut BufReader::new(key_bytes.as_slice()))
            .with_context(|| format!("Failed to parse TLS key: {}", key_path.display()))?
            .context("No private key found in TLS key file")?;

        let signing_key = rustls::crypto::aws_lc_rs::sign::any_supported_type(&key)
            .context("Unsupported TLS private key type")?;

        let certified_key = CertifiedKey::new(certs, signing_key);
        resolver.add(hc.hostname.clone(), Arc::new(certified_key));
    }

    let config = ServerConfig::builder()
        .with_no_client_auth()
        .with_cert_resolver(StdArc::new(resolver));

    Ok(config)
}

// ── Top-level entry point ─────────────────────────────────────────────────────

pub async fn run_server(
    web_config: WebConfig,
    project: String,
    project_path: PathBuf,
    repo_root: PathBuf,
    ready_tx: Option<tokio::sync::oneshot::Sender<()>>,
) -> Result<()> {
    let state = Arc::new(ProxyState::new(
        web_config.hosts.clone(),
        web_config.port,
        web_config.http_port,
        repo_root,
    ));

    let _port = state.add_project(&project, &project_path).await?;

    // ── HTTP app (plain, optionally redirects TLS hosts to HTTPS) ──────────
    //
    // Requests arriving on the HTTP listener have no ViaHttps extension;
    // the proxy handler uses its absence to detect non-TLS connections.
    let http_app = Router::new()
        .route("/api/v1/sessions", get(api_sessions))
        .route("/api/v1/add", post(api_add))
        .route("/api/v1/stop", post(api_stop))
        .route("/api/v1/events", get(api_events))
        .fallback(subdomain_proxy)
        .with_state(Arc::clone(&state));

    // ── HTTPS app (injects ViaHttps(true) so handler knows it's TLS) ───────
    //
    // We need per-request extension injection. Axum's `layer` approach with
    // `Extension` inserts it once per middleware call, which is per-request.
    let https_app = Router::new()
        .route("/api/v1/sessions", get(api_sessions))
        .route("/api/v1/add", post(api_add))
        .route("/api/v1/stop", post(api_stop))
        .route("/api/v1/events", get(api_events))
        .fallback(subdomain_proxy)
        .layer(axum::middleware::from_fn(
            |mut req: Request<Body>, next: axum::middleware::Next| async move {
                let _ = req.extensions_mut().insert(ViaHttps(true));
                next.run(req).await
            },
        ))
        .with_state(Arc::clone(&state));

    let has_tls = web_config.has_tls_hosts();
    let http_bind = format!("{}:{}", web_config.bind, web_config.http_port);
    let https_bind = format!("{}:{}", web_config.bind, web_config.port);

    // Always bind the HTTP listener.
    let http_listener = tokio::net::TcpListener::bind(&http_bind)
        .await
        .with_context(|| format!("Failed to bind HTTP listener on {}", http_bind))?;
    let actual_http_port = http_listener.local_addr()?.port();

    // Write portfile with the HTTP port (always available for the management
    // API and the client-side `meow web` invocations).
    write_portfile(actual_http_port).await?;

    // Log which hosts are active on which listeners.
    for hc in &web_config.hosts {
        if hc.tls_enabled() {
            info!(
                hostname = %hc.hostname,
                "virtual host → https://{}:{}",
                hc.hostname, web_config.port
            );
        } else {
            info!(
                hostname = %hc.hostname,
                "virtual host → http://{}:{}",
                hc.hostname, actual_http_port
            );
        }
    }

    // Shared graceful-shutdown signal.
    let (shutdown_tx, _) = tokio::sync::broadcast::channel::<()>(1);
    let shutdown_tx2 = shutdown_tx.clone();
    let _ctrl_c_task = tokio::spawn(async move {
        let _ = tokio::signal::ctrl_c().await;
        info!("Shutting down...");
        remove_portfile().await;
        // Broadcast shutdown; receivers ignore errors (no subscribers yet is fine).
        let _ = shutdown_tx2.send(());
    });

    // Signal readiness (used by tests).
    if let Some(tx) = ready_tx {
        let _ = tx.send(());
    }

    if has_tls {
        // Build the SNI-capable rustls config from all TLS hosts.
        let tls_hosts = web_config.tls_hosts();
        let rustls_config = build_tls_config(&tls_hosts).await?;
        let axum_tls_config =
            axum_server::tls_rustls::RustlsConfig::from_config(Arc::new(rustls_config));

        let https_addr: std::net::SocketAddr =
            https_bind.parse().context("Invalid HTTPS bind address")?;

        let handle = axum_server::Handle::new();
        let handle2 = handle.clone();
        let mut shutdown_rx = shutdown_tx.subscribe();
        let _shutdown_tls = tokio::spawn(async move {
            let _ = shutdown_rx.recv().await;
            handle2.graceful_shutdown(Some(std::time::Duration::from_secs(5)));
        });

        // Run both listeners concurrently; either finishing ends the server.
        let mut shutdown_rx2 = shutdown_tx.subscribe();
        let http_shutdown = async move {
            let _ = shutdown_rx2.recv().await;
        };

        tokio::select! {
            result = axum_server::bind_rustls(https_addr, axum_tls_config)
                .handle(handle)
                .serve(https_app.into_make_service()) =>
            {
                result.context("HTTPS server error")?;
            }
            result = axum::serve(http_listener, http_app)
                .with_graceful_shutdown(http_shutdown) =>
            {
                result.context("HTTP server error")?;
            }
        }
    } else {
        // No TLS hosts — HTTP only.
        let mut shutdown_rx = shutdown_tx.subscribe();
        let http_shutdown = async move {
            let _ = shutdown_rx.recv().await;
        };

        axum::serve(http_listener, http_app)
            .with_graceful_shutdown(http_shutdown)
            .await
            .context("HTTP server error")?;
    }

    Ok(())
}

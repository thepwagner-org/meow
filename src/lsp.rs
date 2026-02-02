//! LSP server for meow markdown diagnostics.
//!
//! Runs the same validation as `meow fmt --check` on file open/save
//! and publishes errors as LSP diagnostics.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use lsp_server::{Connection, Message, Notification as LspNotification};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, InitializeParams, Position, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
};
use tracing::{debug, info, warn};

use crate::markdown::{self, FormatOptions};
use crate::{git, PROJECTS_DIR};

/// Start the LSP server on stdio.
pub fn run() -> Result<()> {
    info!("starting meow LSP server");

    let (connection, io_threads) = Connection::stdio();

    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    };

    let caps_value =
        serde_json::to_value(capabilities).context("failed to serialize server capabilities")?;
    let init_value = connection
        .initialize(caps_value)
        .map_err(|e| anyhow::anyhow!("LSP initialize failed: {e}"))?;
    let _init_params: InitializeParams = serde_json::from_value(init_value)?;

    info!("meow LSP initialized");

    let repo = git::find_repo().context("failed to find git repository")?;
    let root = git::repo_root(&repo).context("failed to get repo root")?;

    main_loop(&connection, &repo, &root)?;
    io_threads
        .join()
        .map_err(|e| anyhow::anyhow!("IO thread error: {e}"))?;

    info!("meow LSP server shut down");
    Ok(())
}

/// Process messages until shutdown.
fn main_loop(connection: &Connection, repo: &git2::Repository, root: &Path) -> Result<()> {
    // Track files with published diagnostics so we can clear stale ones.
    let mut published_files: HashSet<PathBuf> = HashSet::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection
                    .handle_shutdown(&req)
                    .map_err(|e| anyhow::anyhow!("shutdown error: {e}"))?
                {
                    return Ok(());
                }
            }
            Message::Notification(not) => {
                handle_notification(connection, &not, repo, root, &mut published_files);
            }
            Message::Response(_) => {}
        }
    }

    Ok(())
}

/// Dispatch incoming notifications.
fn handle_notification(
    connection: &Connection,
    not: &LspNotification,
    repo: &git2::Repository,
    root: &Path,
    published_files: &mut HashSet<PathBuf>,
) {
    match not.method.as_str() {
        "textDocument/didOpen" | "textDocument/didSave" | "textDocument/didChange" => {
            if let Some(path) = extract_file_path(not) {
                if path.extension().is_some_and(|e| e == "md") {
                    if let Err(e) =
                        validate_and_publish(connection, &path, repo, root, published_files)
                    {
                        warn!("validation failed for {}: {e:#}", path.display());
                    }
                }
            }
        }
        _ => {}
    }
}

/// Run project validation and publish diagnostics for all files.
fn validate_and_publish(
    connection: &Connection,
    file_path: &Path,
    repo: &git2::Repository,
    root: &Path,
    published_files: &mut HashSet<PathBuf>,
) -> Result<()> {
    let project = detect_project(root, file_path).context("file is not inside a project")?;

    debug!(
        "validating project {project} (triggered by {})",
        file_path.display()
    );

    let git_tree = git::list_all_paths(repo)?;
    let opts = FormatOptions {
        skip_encrypted: true,
        check: true,
    };

    let result = markdown::format_project(root, &project, Some(&git_tree), opts)?;

    // Publish diagnostics for files with errors.
    let mut new_published: HashSet<PathBuf> = HashSet::new();

    for file_error in &result.errors {
        let error_path = PathBuf::from(&file_error.path);
        let diagnostics: Vec<Diagnostic> = file_error
            .errors
            .iter()
            .map(|e| {
                // ValidationError.line is 1-based (0 = unknown); LSP is 0-based.
                let line = if e.line() > 0 { e.line() - 1 } else { 0 };
                Diagnostic {
                    range: Range {
                        start: Position {
                            line: line as u32,
                            character: 0,
                        },
                        end: Position {
                            line: line as u32,
                            character: 0,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("meow".to_string()),
                    message: e.message(),
                    ..Default::default()
                }
            })
            .collect();

        publish_diagnostics(connection, &error_path, diagnostics)?;
        let _ = new_published.insert(error_path);
    }

    // Clear diagnostics for files that previously had errors but are now clean.
    for old_path in published_files.difference(&new_published) {
        publish_diagnostics(connection, old_path, vec![])?;
    }

    *published_files = new_published;

    debug!(
        "published diagnostics: {} files checked, {} with errors",
        result.files_checked,
        result.errors.len()
    );

    Ok(())
}

/// Extract the project name from a file path under `{root}/projects/{name}/...`.
fn detect_project(root: &Path, file_path: &Path) -> Option<String> {
    let projects_dir = root.join(PROJECTS_DIR);
    let relative = file_path.strip_prefix(&projects_dir).ok()?;
    let project = relative.components().next()?;
    Some(project.as_os_str().to_string_lossy().to_string())
}

/// Pull the file URI out of a didOpen or didSave notification.
fn extract_file_path(not: &LspNotification) -> Option<PathBuf> {
    let uri = not
        .params
        .as_object()?
        .get("textDocument")?
        .as_object()?
        .get("uri")?
        .as_str()?;
    uri_to_path(uri)
}

/// Convert a `file://` URI to a filesystem path.
fn uri_to_path(uri: &str) -> Option<PathBuf> {
    uri.strip_prefix("file://").map(PathBuf::from)
}

/// Convert a filesystem path to a `file://` URI.
fn path_to_uri(path: &Path) -> Result<Uri> {
    format!("file://{}", path.display())
        .parse()
        .map_err(|e| anyhow::anyhow!("invalid URI for {}: {e}", path.display()))
}

/// Send a publishDiagnostics notification.
fn publish_diagnostics(
    connection: &Connection,
    path: &Path,
    diagnostics: Vec<Diagnostic>,
) -> Result<()> {
    let params = PublishDiagnosticsParams {
        uri: path_to_uri(path)?,
        diagnostics,
        version: None,
    };
    connection
        .sender
        .send(Message::Notification(LspNotification::new(
            "textDocument/publishDiagnostics".to_string(),
            params,
        )))?;
    Ok(())
}

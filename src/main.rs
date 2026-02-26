use anyhow::Result;
use clap::Parser;
use meow::cli::{Cli, Command};
use meow::{color, commands, config, git, lsp};
use std::io::Write;
use std::process::ExitCode;
use tracing::error;

fn main() -> ExitCode {
    // Install the rustls crypto provider once for the whole process.
    // aws-lc-rs is already compiled in via axum-server; this resolves the
    // conflict with reqwest which uses rustls-tls-no-provider.
    let _ = rustls::crypto::aws_lc_rs::default_provider().install_default();

    // Skip tracing setup for LSP (uses stdio for protocol)
    if !std::env::args().any(|a| a == "lsp") {
        let is_web = std::env::args().any(|a| a == "web");
        let log_file = if is_web { open_log_file() } else { None };

        if let Some(file) = log_file {
            let tee = TeeWriter {
                stderr: std::io::stderr(),
                file,
            };
            tracing_subscriber::fmt()
                .with_env_filter(
                    tracing_subscriber::EnvFilter::try_from_default_env()
                        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("meow=info")),
                )
                .with_writer(std::sync::Mutex::new(tee))
                .with_target(false)
                .with_ansi(false)
                .init();
        } else {
            tracing_subscriber::fmt()
                .with_env_filter(
                    tracing_subscriber::EnvFilter::try_from_default_env()
                        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("meow=info")),
                )
                .with_writer(std::io::stderr)
                .with_target(false)
                .init();
        }
    }

    match run() {
        Ok(None) => ExitCode::SUCCESS,
        Ok(Some(code)) => ExitCode::from(code),
        Err(e) => {
            error!("{e:#}");
            ExitCode::FAILURE
        }
    }
}

/// Returns Ok(None) for success, Ok(Some(code)) for non-error exit codes, Err for failures.
fn run() -> Result<Option<u8>> {
    let cli = Cli::parse();
    let repo = git::find_repo()?;
    let root = git::repo_root(&repo)?;

    // Parse color choice and determine if colors should be used
    let color_choice: color::ColorChoice =
        cli.color.parse().map_err(|e| anyhow::anyhow!("{}", e))?;
    let use_color = color::should_use_color(color_choice);

    match cli.command {
        Command::List { all } => commands::cmd_list(&repo, &root, all, use_color).map(|()| None),

        Command::Add { project, create } => {
            let path = commands::cmd_add(&root, &project, create)?;
            #[allow(clippy::print_stdout)]
            {
                println!("{}", path.display());
            }
            Ok(None)
        }

        Command::Rm { project } => commands::cmd_rm(&repo, &root, &project).map(|()| None),

        Command::Cd { query } => {
            if let Some(path) = commands::cmd_cd(&repo, &root, query, use_color)? {
                #[allow(clippy::print_stdout)]
                {
                    println!("{}", path.display());
                }
            }
            Ok(None)
        }

        Command::Init { shell } => {
            let script = commands::cmd_init(&shell)?;
            #[allow(clippy::print_stdout)]
            {
                println!("{script}");
            }
            Ok(None)
        }

        Command::Fmt(args) => commands::cmd_fmt(&repo, &root, args).map(|()| None),

        Command::Journal(args) => {
            commands::cmd_journal(&repo, &root, args, use_color).map(|()| None)
        }

        Command::Zellij {
            query,
            branch_parts,
            layout,
            worktree,
        } => commands::cmd_zellij(&root, query, branch_parts, &layout, worktree, use_color)
            .map(|()| None),

        Command::Prune(args) => commands::cmd_prune(&root, args).map(|()| None),

        Command::Decrypt { file, in_place } => {
            commands::cmd_decrypt(&file, in_place).map(|()| None)
        }

        Command::Pull => commands::cmd_pull(&root).map(|()| None),

        Command::Mirror { command } => commands::cmd_mirror(&root, command, use_color),

        Command::Lsp => lsp::run().map(|()| None),

        Command::Web { query, command } => {
            commands::cmd_web(&repo, &root, query, command, use_color)
        }
    }
}

// ── Log file with rotation ───────────────────────────────────────────────────

/// Opens `~/.local/share/meow/web.log`, rotating previous logs first.
/// Keeps at most 3 old logs (`web.log.1`, `web.log.2`, `web.log.3`).
/// Returns `None` if the data dir can't be created or the file can't be opened.
fn open_log_file() -> Option<std::fs::File> {
    let dir = config::data_dir().ok()?;
    std::fs::create_dir_all(&dir).ok()?;
    let base = dir.join("web.log");

    // Rotate: web.log.3 is dropped, web.log.2 → .3, web.log.1 → .2, web.log → .1
    for i in (1..3).rev() {
        let from = dir.join(format!("web.log.{i}"));
        let to = dir.join(format!("web.log.{}", i + 1));
        let _ = std::fs::rename(&from, &to);
    }
    if base.exists() {
        let _ = std::fs::rename(&base, dir.join("web.log.1"));
    }

    std::fs::File::create(&base).ok()
}

/// Writer that tees output to both stderr and a log file.
struct TeeWriter {
    stderr: std::io::Stderr,
    file: std::fs::File,
}

impl Write for TeeWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let _ = self.file.write_all(buf);
        self.stderr.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        let _ = self.file.flush();
        self.stderr.flush()
    }
}

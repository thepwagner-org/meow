use anyhow::Result;
use clap::Parser;
use meow::cli::{Cli, Command, QmdCommand};
use meow::{color, commands, git};
use std::process::ExitCode;
use tracing::error;

fn main() -> ExitCode {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("meow=info")),
        )
        .with_writer(std::io::stderr)
        .with_target(false)
        .init();

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

        Command::Journal(args) => {
            commands::cmd_journal(&repo, &root, args, use_color).map(|()| None)
        }

        Command::Zellij {
            query,
            branch_parts,
            layout,
            worktree,
            prompt,
            command,
        } => commands::cmd_zellij(
            &root,
            query,
            branch_parts,
            &layout,
            worktree,
            prompt,
            command,
            use_color,
        )
        .map(|()| None),

        Command::Prune(args) => commands::cmd_prune(&root, args).map(|()| None),

        Command::Pull => commands::cmd_pull(&root).map(|()| None),

        Command::Mirror { command } => commands::cmd_mirror(&root, command, use_color),

        Command::Qmd { command } => match command {
            Some(QmdCommand::Status) => {
                commands::cmd_qmd_status(&repo, &root, use_color).map(|()| None)
            }
            None => commands::cmd_qmd_sync(&repo, &root).map(|()| None),
        },

        Command::Please {
            projects,
            profile,
            branch,
            no_cleanup,
        } => commands::cmd_agent(
            &repo, &root, projects, profile, branch, no_cleanup, use_color,
        )
        .map(|()| None),
    }
}

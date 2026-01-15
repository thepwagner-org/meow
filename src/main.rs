use anyhow::Result;
use clap::Parser;
use meow::cli::{Cli, Command};
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
        .without_time()
        .with_target(false)
        .init();

    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            error!("{e:#}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<()> {
    let cli = Cli::parse();
    let repo = git::find_repo()?;
    let root = git::repo_root(&repo)?;

    // Parse color choice and determine if colors should be used
    let color_choice: color::ColorChoice =
        cli.color.parse().map_err(|e| anyhow::anyhow!("{}", e))?;
    let use_color = color::should_use_color(color_choice);

    match cli.command {
        Command::List { all } => commands::cmd_list(&repo, &root, all, use_color),

        Command::Add { project, create } => {
            let path = commands::cmd_add(&root, &project, create)?;
            #[allow(clippy::print_stdout)]
            {
                println!("{}", path.display());
            }
            Ok(())
        }

        Command::Rm { project } => commands::cmd_rm(&repo, &root, &project),

        Command::Cd { query } => {
            if let Some(path) = commands::cmd_cd(&repo, &root, query, use_color)? {
                #[allow(clippy::print_stdout)]
                {
                    println!("{}", path.display());
                }
            }
            Ok(())
        }

        Command::Init { shell } => {
            let script = commands::cmd_init(&shell)?;
            #[allow(clippy::print_stdout)]
            {
                println!("{script}");
            }
            Ok(())
        }

        Command::Fmt(args) => commands::cmd_fmt(&repo, &root, args),

        Command::Journal(args) => commands::cmd_journal(&repo, &root, args, use_color),

        Command::Zellij {
            query,
            branch_parts,
            layout,
            worktree,
        } => commands::cmd_zellij(&root, query, branch_parts, &layout, worktree, use_color),

        Command::Prune(args) => commands::cmd_prune(&root, args),

        Command::Decrypt { file, in_place } => commands::cmd_decrypt(&file, in_place),

        Command::Pull => commands::cmd_pull(&root),
    }
}

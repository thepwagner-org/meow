use std::path::PathBuf;

use chrono::NaiveDate;
use clap::{Args, Parser, Subcommand};

#[derive(Parser, Debug)]
#[command(name = "meow")]
#[command(about = "My Engine for Organizing Work", version)]
pub struct Cli {
    /// Control colored output
    #[arg(
        long,
        global = true,
        value_name = "WHEN",
        default_value = "auto",
        help = "When to use colors (auto, always, never)"
    )]
    pub color: String,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Subcommand, Debug)]
pub enum Command {
    /// List projects
    #[command(alias = "ls")]
    List {
        /// Show all projects, not just focused ones
        #[arg(short = 'A', long)]
        all: bool,
    },

    /// Add a project to sparse checkout
    Add {
        /// Project name (directory under projects/)
        project: String,
        /// Create new project with boilerplate files
        #[arg(long)]
        create: bool,
    },

    /// Remove a project from sparse checkout
    #[command(alias = "drop")]
    Rm {
        /// Project name to remove from focus
        project: String,
    },

    /// Fuzzy find a project, add to focus, and print path (for shell cd)
    #[command(hide = true)]
    Cd {
        /// Search query (fuzzy matched against project names)
        query: Option<String>,
    },

    /// Output shell initialization script
    #[command(hide = true)]
    Init {
        /// Shell type (bash, zsh, fish)
        shell: String,
    },

    /// Format markdown files
    Fmt(FmtArgs),

    /// Read project journal entries
    Journal(JournalArgs),

    /// Open or switch to a zellij tab for a project
    #[command(alias = "z")]
    Zellij {
        /// Project name or fuzzy query
        query: Option<String>,
        /// Branch name parts (kebab-cased together, implies worktree)
        #[arg(trailing_var_arg = true)]
        branch_parts: Vec<String>,
        /// Layout to use for new tabs
        #[arg(short, long, default_value = "dev")]
        layout: String,
        /// Create a fresh git worktree with timestamp-based branch
        #[arg(short = 't', long, visible_aliases = ["wt", "tree"])]
        worktree: bool,
    },

    /// Prune stale git worktrees
    Prune(PruneArgs),

    /// Decrypt an encrypted file
    Decrypt {
        /// File path to decrypt
        file: PathBuf,
        /// Decrypt in place (modify file)
        #[arg(short, long)]
        in_place: bool,
    },

    /// Pull latest changes in the monorepo
    Pull,

    /// GitHub mirror management
    Mirror {
        #[command(subcommand)]
        command: MirrorCommand,
    },
}

#[derive(Subcommand, Debug)]
pub enum MirrorCommand {
    /// Show status of all mirrored projects
    Status,

    /// Prepare mirror for a project (clone, sync, show path)
    Diff {
        /// Project name (defaults to current directory's project)
        project: Option<String>,
    },
}

#[derive(Args, Debug)]
pub struct PruneArgs {
    /// Skip confirmation for dirty worktrees
    #[arg(short, long)]
    pub force: bool,
}

#[derive(Args, Debug)]
pub struct FmtArgs {
    /// Project name (defaults to current directory's project)
    #[arg(conflicts_with = "all")]
    pub project: Option<String>,

    /// Format all active (focused) projects
    #[arg(short = 'A', long)]
    pub all: bool,

    /// Skip encrypted files (don't decrypt/re-encrypt)
    #[arg(long)]
    pub skip_encrypted: bool,
}

#[derive(Args, Debug)]
pub struct JournalArgs {
    /// Project name (defaults to current directory's project)
    #[arg(conflicts_with = "all")]
    pub project: Option<String>,

    /// Show journal entries for all active projects
    #[arg(short = 'A', long)]
    pub all: bool,

    /// Include git commits interleaved with journal entries
    #[arg(short = 'g', long)]
    pub git: bool,

    /// Show entries from this date onwards (YYYY-MM-DD)
    #[arg(long)]
    pub since: Option<NaiveDate>,

    /// Show entries until this date (YYYY-MM-DD)
    #[arg(long)]
    pub until: Option<NaiveDate>,

    /// Show entries from the last N days
    #[arg(long, conflicts_with_all = ["since", "until"])]
    pub days: Option<u32>,
}

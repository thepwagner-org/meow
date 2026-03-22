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
        /// Start opencode in the new tab and send a free-form prompt
        #[arg(long)]
        prompt: Option<String>,
        /// Start opencode in the new tab and run a slash command (e.g. "shipit-mirror")
        #[arg(long)]
        command: Option<String>,
    },

    /// Prune stale git worktrees
    Prune(PruneArgs),

    /// Pull latest changes in the monorepo
    Pull,

    /// GitHub mirror management
    Mirror {
        #[command(subcommand)]
        command: MirrorCommand,
    },

    /// Sync QMD search index collections with focused projects
    Qmd {
        #[command(subcommand)]
        command: Option<QmdCommand>,
    },

    /// Start an agent session for one or more projects in nix-jail
    #[command(alias = "plz")]
    Please {
        /// Project names (omit for interactive picker)
        projects: Vec<String>,
        /// Profile names to use (default: auto-detect from project files)
        #[arg(long, short)]
        profile: Vec<String>,
        /// Git ref (branch, tag, or SHA) to check out
        #[arg(long)]
        branch: Option<String>,
        /// Skip sandbox cleanup after the job exits (preserves root/workspace for inspection)
        #[arg(long)]
        no_cleanup: bool,
    },
}

#[derive(Subcommand, Debug)]
pub enum QmdCommand {
    /// Show which focused projects are indexed in QMD
    Status,
}

#[derive(Subcommand, Debug)]
pub enum MirrorCommand {
    /// Show status of all mirrored projects
    Status,

    /// Prepare mirror for a project (clone, sync, show path)
    Diff {
        /// Project name (defaults to current directory's project)
        project: Option<String>,

        /// Skip secret scanning with trufflehog
        #[arg(long)]
        no_scan: bool,
    },

    /// Commit, push, and tag a prepared mirror
    Push {
        /// Project name (defaults to current directory's project)
        project: Option<String>,

        /// Commit message
        #[arg(long, short)]
        message: String,
    },
}

#[derive(Args, Debug)]
pub struct PruneArgs {
    /// Skip confirmation for dirty worktrees
    #[arg(short, long)]
    pub force: bool,
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

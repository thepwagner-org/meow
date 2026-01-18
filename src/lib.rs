pub mod cli;
pub mod color;
pub mod commands;
pub mod config;
pub mod crypto;
pub mod git;
pub mod markdown;
pub mod mirror;
pub mod picker;
pub mod sparse;

/// Directory name for projects within the monorepo
pub const PROJECTS_DIR: &str = "projects";

/// Directory name for journal entries within a project
pub const JOURNAL_DIR: &str = "journal";

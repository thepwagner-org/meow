# meow

CLI tool for managing sparse-checkout in monorepos. Focus on the projects you're actively working on.

## Usage

```bash
# List focused projects
meow list

# List all projects (focused marked with *)
meow list --all

# Add a project to focus
meow add <project>

# Remove a project from focus
meow drop <project>

# Create a new project with boilerplate
meow add --create <project>

# Format markdown files in current project
meow fmt

# Read journal entries
meow journal
meow journal --days 7
meow journal --git  # interleave with commits

# Open project in zellij tab
meow z <project>
meow z <project> feature-name  # in new worktree

# Maintenance
meow pull           # git pull
meow prune          # clean up worktrees
meow decrypt <file> # decrypt encrypted markdown

# Start LSP server (markdown diagnostics)
meow lsp
```

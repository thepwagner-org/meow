# meow

CLI for navigating a monorepo of projects. If you're reading this on GitHub, it arrived here via `meow mirror push`.

## Setup

```bash
# Add to fish config
meow init fish | source
```

This gives you `m` for `meow`, `mz` for `meow zellij`, and `mw` for `meow web`. Any unrecognized subcommand becomes a fuzzy project jump — `meow foo` finds and `cd`s to the best match.

## Sparse Checkout

```bash
# List focused projects
meow list

# List everything
meow list --all

# Focus on a project (and cd to it)
meow add <project>

# Create a new project with boilerplate
meow add --create <project>

# Remove from focus
meow rm <project>
```

## Zellij Sessions

```bash
# Open project in a zellij tab
meow z <project>

# Open in a fresh worktree
meow z <project> feature-name

# Worktree with a timestamp branch
meow z <project> -t

# Clean up stale worktrees
meow prune
```

Uses `.meow.d/layout.kdl` from the project if present, otherwise the `dev` layout.

## OpenCode Web

```bash
# Open a project in the browser
meow web <project>

# List active sessions
meow web list

# Stop a session
meow web stop <project>
```

Each project gets a subdomain on your hostname via a local proxy.

## Journal

```bash
# Current project
meow journal

# Last 7 days
meow journal --days 7

# Interleave with git commits
meow journal --git

# All focused projects
meow journal --all
```

## GitHub Mirrors

How this README got here, probably.

```bash
# Check status across all mirrored projects
meow mirror status

# Prepare a mirror (sync + secret scan)
meow mirror diff

# Push to GitHub
meow mirror push -m "sync changes"

# Pull latest changes in the monorepo
meow pull
```

# meow Roadmap

This document explores future possibilities for meow. For current implementation and development guidelines, see [CLAUDE.md](CLAUDE.md).

## Shell Support

Currently only fish is supported. Could expand to bash and zsh.
**Possible approaches:**
- `meow init bash` / `meow init zsh` with shell-specific wrapper functions
- Detect shell from `$SHELL` environment variable
- POSIX-compatible fallback for unsupported shells
**Considerations:**
- Fish has better interactive features (completions, syntax)
- Bash/zsh may be needed for CI or unfamiliar machines
- Keep wrapper functions minimal - heavy logic stays in Rust

## Journal Authoring

Currently `meow journal` is read-only. Could add entry creation.
**Possible approaches:**
- `meow journal --add "entry text"` appends to today's section
- `meow journal --edit` opens editor at current date
- Auto-create month file and date heading if missing
**Considerations:**
- Keep it simple - journal is markdown, not a database
- Defer complex editing to the editor; meow just positions cursor
- Timestamp granularity: daily (current) vs hourly entries

## Project Templates

`meow add --create` generates basic boilerplate (README, CLAUDE.md, shell.nix, .envrc). Could support language-specific templates.
**Possible approaches:**
- `meow add --create --template rust` for Cargo.toml, src/
- `meow add --create --template go` for go.mod
- Templates as directories in `~/.config/meow/templates/`
**Considerations:**
- Nix shell is always created (it's the common substrate)
- Language templates layer on top
- Keep defaults minimal - don't over-engineer

## Non-Goals

Explicitly out of scope to keep the project focused:
- **Life management** - Stays in MTD; meow is project navigation only
- **Financial tracking** - MTD handles this
- **Time tracking** - No Pomodoro, no time estimates
- **Task prioritization** - No kanban, no project states
- **Collaboration features** - Single-user tool
- **GUI** - CLI-first, shell integration
- **Note types beyond project metadata** - README, CLAUDE, journal, ROADMAP only

## See Also

- [CLAUDE.md](CLAUDE.md) - Development guidelines and architecture

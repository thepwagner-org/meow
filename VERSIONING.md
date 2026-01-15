# Versioning with Crane Cache Preservation

This project uses a split versioning approach to preserve Nix/Crane dependency caches across version bumps.

## Structure

- `version.toml` - Contains the real version (e.g., `version = "1.2.3"`)
- `Cargo.toml` - Contains a static placeholder `version = "0.0.0-dev"`
- `package.nix` - Uses `buildRustPackage` helper from `nix/lib.nix`

## Why

Crane's `buildDepsOnly` caches compiled dependencies based on Cargo.toml/Cargo.lock hashes. When the version changes in Cargo.toml, the cache is invalidated and all dependencies rebuild.

By keeping Cargo.toml static and patching the version only for the final build:
- `buildDepsOnly` always uses the same source hash → cache hit
- `buildPackage` gets the real version patched in → correct binary version

## Prompt for Converting Other Projects

```
Convert this Rust project to use split versioning for Crane cache preservation:

1. Create `version.toml` with the current version:
   ```toml
   version = "X.Y.Z"
   ```

2. Update `Cargo.toml` to use placeholder:
   ```toml
   version = "0.0.0-dev"
   ```

3. Run `cargo build` to update Cargo.lock with the placeholder

4. Update `package.nix` to use the `buildRustPackage` helper:
   ```nix
   {
     pkgs,
     buildRustPackage,
     ...
   }:
   buildRustPackage {
     src = ./.;
     extraArgs = {
       # your build inputs here
     };
   }
   ```

   The helper (from `nix/lib.nix`) automatically:
   - Reads version from `version.toml`
   - Patches it into Cargo.toml/Cargo.lock for the final build
   - Uses the placeholder version for dependency caching

5. Update any CI/hooks to bump `version.toml` instead of `Cargo.toml`
```

## Version Bumps

To release a new version, only edit `version.toml`. Cargo.toml and Cargo.lock stay unchanged.

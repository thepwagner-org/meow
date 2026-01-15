# Encrypted Markdown Support

Add transparent encryption support to meow for sensitive documents.

## File Format

```yaml
---
type: health/weight-log
encrypted: true
key-id: 0xABCD1234
---
<base64-encoded encrypted blob>
```

- **Frontmatter**: Always plaintext YAML. New fields:
  - `encrypted: true` - marks file as encrypted
  - `key-id: <GPG key ID>` - which GPG key encrypts the symmetric key
- **Body**: AES-256-GCM encrypted, then base64 encoded. Fresh random IV per write.

## Cryptographic Design

1. **Symmetric key per file** - generate random 256-bit key on first encryption
2. **Key wrapped with GPG** - symmetric key encrypted to the specified GPG key ID, stored in a sidecar or header
3. **Yubikey as trust anchor** - GPG key lives on hardware token, gpg-agent handles unlock/caching

Option A (simpler): Store wrapped symmetric key in frontmatter as `wrapped-key: <base64>`.

Option B (cleaner): Each encrypted blob is just `gpg --encrypt --recipient <key-id>` output. No separate symmetric key management - let GPG handle it. Slightly slower but fewer moving parts.

Start with Option B unless performance is a problem.

## Commands

### `meow edit <path>`

Primary interface for encrypted files:

1. Check `encrypted: true` in frontmatter
2. Decrypt body to secure temp file (mode 0600, in `$TMPDIR`)
3. Open `$EDITOR` on temp file
4. On editor exit: re-encrypt, update file, shred temp file
5. If file unchanged, skip re-encryption

For non-encrypted files, just open `$EDITOR` directly.

### `meow fmt` changes

When processing encrypted files:

1. Decrypt body to memory
2. Run existing formatting logic on plaintext
3. Re-encrypt and write back
4. Fresh IV means file always changes in git - acceptable tradeoff

Add `--skip-encrypted` flag to leave encrypted files untouched.

### `meow new --encrypt`

When creating new documents:

1. `--encrypt` flag triggers encryption
2. Prompt for or use default GPG key ID
3. Create file with `encrypted: true` and empty encrypted body
4. Immediately open in `meow edit` flow

## Implementation Notes

- Use `gpgme` crate or shell out to `gpg` CLI (CLI is simpler, gpgme more robust)
- Temp files: use `tempfile` crate with secure deletion
- Base64: use `base64` crate, standard encoding
- Detect encrypted files early in parsing pipeline, decrypt before schema validation

## Testing

- Unit tests with test GPG keys (not hardware-bound)
- Integration tests that verify round-trip: encrypt → format → decrypt yields valid markdown
- Test that frontmatter survives encryption cycles unchanged

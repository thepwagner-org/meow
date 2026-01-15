//! Encryption support for sensitive markdown documents.
//!
//! Uses AES-256-GCM for content encryption with per-file symmetric keys.
//! Symmetric keys are wrapped with GPG public keys for storage.

use aes_gcm::{
    aead::{Aead, KeyInit},
    Aes256Gcm, Nonce,
};
use anyhow::{bail, Context, Result};
use base64::prelude::*;
use rand::RngCore;
use std::process::Command;

/// AES-256 key size in bytes
const KEY_SIZE: usize = 32;

/// AES-GCM nonce size in bytes
const NONCE_SIZE: usize = 12;

/// Generate a random 256-bit AES key
pub fn generate_key() -> [u8; KEY_SIZE] {
    let mut key = [0u8; KEY_SIZE];
    rand::thread_rng().fill_bytes(&mut key);
    key
}

/// Encrypt plaintext with AES-256-GCM.
///
/// Returns a blob containing: nonce (12 bytes) || ciphertext || auth tag (16 bytes)
pub fn encrypt_aes_gcm(plaintext: &[u8], key: &[u8]) -> Result<Vec<u8>> {
    if key.len() != KEY_SIZE {
        bail!("Invalid key size: expected {KEY_SIZE}, got {}", key.len());
    }

    let cipher =
        Aes256Gcm::new_from_slice(key).map_err(|_| anyhow::anyhow!("Invalid key length"))?;

    // Generate random nonce
    let mut nonce_bytes = [0u8; NONCE_SIZE];
    rand::thread_rng().fill_bytes(&mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);

    // Encrypt
    let ciphertext = cipher
        .encrypt(nonce, plaintext)
        .map_err(|e| anyhow::anyhow!("Encryption failed: {e}"))?;

    // Prepend nonce to ciphertext
    let mut result = Vec::with_capacity(NONCE_SIZE + ciphertext.len());
    result.extend_from_slice(&nonce_bytes);
    result.extend_from_slice(&ciphertext);

    Ok(result)
}

/// Decrypt ciphertext with AES-256-GCM.
///
/// Expects blob format: nonce (12 bytes) || ciphertext || auth tag (16 bytes)
pub fn decrypt_aes_gcm(blob: &[u8], key: &[u8]) -> Result<Vec<u8>> {
    if key.len() != KEY_SIZE {
        bail!("Invalid key size: expected {KEY_SIZE}, got {}", key.len());
    }

    if blob.len() < NONCE_SIZE {
        bail!("Ciphertext too short: missing nonce");
    }

    let cipher =
        Aes256Gcm::new_from_slice(key).map_err(|_| anyhow::anyhow!("Invalid key length"))?;

    // Split nonce and ciphertext
    let (nonce_bytes, ciphertext) = blob.split_at(NONCE_SIZE);
    let nonce = Nonce::from_slice(nonce_bytes);

    // Decrypt
    cipher
        .decrypt(nonce, ciphertext)
        .map_err(|e| anyhow::anyhow!("Decryption failed: {e}"))
}

/// Wrap (encrypt) an AES key with a GPG public key.
///
/// Shells out to `gpg --encrypt --recipient <key_id>`.
/// Returns base64-encoded wrapped key.
pub fn wrap_key_gpg(key: &[u8], recipient: &str) -> Result<String> {
    let mut child = Command::new("gpg")
        .args(["--encrypt", "--armor", "--recipient", recipient])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .context("Failed to spawn gpg")?;

    // Write key to stdin
    use std::io::Write;
    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(key)
            .context("Failed to write to gpg stdin")?;
    }

    let output = child.wait_with_output().context("Failed to run gpg")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("gpg encrypt failed: {stderr}");
    }

    // GPG --armor output is already ASCII, but we base64 encode it for consistency
    Ok(BASE64_STANDARD.encode(&output.stdout))
}

/// Unwrap (decrypt) an AES key with GPG.
///
/// Shells out to `gpg --decrypt`. Requires access to the private key
/// (typically via Yubikey/gpg-agent).
/// Input should be base64-encoded GPG-encrypted data.
pub fn unwrap_key_gpg(wrapped_b64: &str) -> Result<Vec<u8>> {
    let wrapped = BASE64_STANDARD
        .decode(wrapped_b64)
        .context("Invalid base64 in wrapped key")?;

    let mut child = Command::new("gpg")
        .args(["--decrypt", "--quiet"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .context("Failed to spawn gpg")?;

    // Write wrapped key to stdin
    use std::io::Write;
    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(&wrapped)
            .context("Failed to write to gpg stdin")?;
    }

    let output = child.wait_with_output().context("Failed to run gpg")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("gpg decrypt failed: {stderr}");
    }

    Ok(output.stdout)
}

/// Encode binary data as base64
pub fn encode_base64(data: &[u8]) -> String {
    BASE64_STANDARD.encode(data)
}

/// Decode base64 to binary data
pub fn decode_base64(encoded: &str) -> Result<Vec<u8>> {
    BASE64_STANDARD
        .decode(encoded)
        .context("Invalid base64 encoding")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aes_roundtrip() {
        let key = generate_key();
        let plaintext = b"Hello, encrypted world!";

        let encrypted = encrypt_aes_gcm(plaintext, &key).expect("encrypt");
        let decrypted = decrypt_aes_gcm(&encrypted, &key).expect("decrypt");

        assert_eq!(plaintext.as_slice(), decrypted.as_slice());
    }

    #[test]
    fn test_different_nonces() {
        let key = generate_key();
        let plaintext = b"Same message";

        let encrypted1 = encrypt_aes_gcm(plaintext, &key).expect("encrypt 1");
        let encrypted2 = encrypt_aes_gcm(plaintext, &key).expect("encrypt 2");

        // Different encryptions should produce different ciphertexts (different nonces)
        assert_ne!(encrypted1, encrypted2);

        // But both should decrypt to the same plaintext
        let decrypted1 = decrypt_aes_gcm(&encrypted1, &key).expect("decrypt 1");
        let decrypted2 = decrypt_aes_gcm(&encrypted2, &key).expect("decrypt 2");
        assert_eq!(decrypted1, decrypted2);
    }

    #[test]
    fn test_wrong_key_fails() {
        let key1 = generate_key();
        let key2 = generate_key();
        let plaintext = b"Secret message";

        let encrypted = encrypt_aes_gcm(plaintext, &key1).expect("encrypt");
        let result = decrypt_aes_gcm(&encrypted, &key2);

        assert!(result.is_err());
    }

    #[test]
    fn test_tampered_ciphertext_fails() {
        let key = generate_key();
        let plaintext = b"Do not tamper";

        let mut encrypted = encrypt_aes_gcm(plaintext, &key).expect("encrypt");
        // Flip a bit in the ciphertext
        if let Some(byte) = encrypted.get_mut(NONCE_SIZE + 5) {
            *byte ^= 0x01;
        }

        let result = decrypt_aes_gcm(&encrypted, &key);
        assert!(result.is_err());
    }

    #[test]
    fn test_base64_roundtrip() {
        let data = b"Binary \x00\xff data";
        let encoded = encode_base64(data);
        let decoded = decode_base64(&encoded).expect("decode");
        assert_eq!(data.as_slice(), decoded.as_slice());
    }
}

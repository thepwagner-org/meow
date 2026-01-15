{pkgs, ...}:
pkgs.mkShell {
  buildInputs = [
    pkgs.cargo
    pkgs.rustfmt
    pkgs.clippy
    pkgs.pkg-config
    pkgs.openssl
    pkgs.git
  ];
}

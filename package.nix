{
  pkgs,
  buildRustPackage,
  ...
}:
buildRustPackage {
  src = ./.;
  extraArgs = {
    strictDeps = true;
    nativeBuildInputs = [pkgs.pkg-config pkgs.makeWrapper];
    buildInputs = [pkgs.openssl];
    nativeCheckInputs = [pkgs.git];
    postInstall = ''
      # Only wrap if binary exists (not during deps-only build)
      if [ -f $out/bin/meow ]; then
        wrapProgram $out/bin/meow \
          --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.trufflehog]}
      fi
    '';
  };
}

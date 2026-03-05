{
  config,
  lib,
  pkgs,
  monorepoPackages,
  ...
}: let
  cfg = config.services.meow-web;

  hostToYaml = host:
    "    - hostname: ${host.hostname}\n"
    + lib.optionalString (host.tlsCert != null) "      tls_cert: ${host.tlsCert}\n"
    + lib.optionalString (host.tlsKey != null) "      tls_key: ${host.tlsKey}\n";

  # Indented 2 spaces to nest under the `web:` key in the generated YAML.
  # Nix strips the common leading whitespace from multiline strings, so the
  # 4-space prefix here becomes 2 spaces in the output.
  sandboxYaml =
    if cfg.sandbox.enable
    then "  sandbox:\n    enabled: true\n    nj_bin: ${cfg.sandbox.njBin}\n    alice_bin: ${cfg.sandbox.aliceBin}\n    sandbox_package: \"${cfg.sandbox.sandboxPackage}\"\n    nix_jail_url: \"${cfg.sandbox.nixJailUrl}\"\n"
    else "";

  configFile = pkgs.writeText "meow-config.yaml" ''
    repo_path: ${cfg.repoPath}

    web:
      bind: "${cfg.bind}"
      port: ${toString cfg.port}
      http_port: ${toString cfg.httpPort}
      hosts:
    ${lib.concatMapStrings hostToYaml cfg.hosts}
    ${sandboxYaml}
  '';

  homeDir = "/var/lib/${cfg.stateDirectory}";
  configDir = "${homeDir}/.config/meow";
in {
  options.services.meow-web = {
    enable = lib.mkEnableOption "meow web proxy";

    package = lib.mkOption {
      type = lib.types.package;
      default = monorepoPackages.meow;
      description = "The meow package to use";
    };

    repoPath = lib.mkOption {
      type = lib.types.str;
      description = "Path to the monorepo root (e.g., /home/pwagner/src)";
    };

    bind = lib.mkOption {
      type = lib.types.str;
      default = "0.0.0.0";
      description = "IP address to bind the server on";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 3443;
      description = "HTTPS port (only used when at least one host has TLS)";
    };

    httpPort = lib.mkOption {
      type = lib.types.port;
      default = 3080;
      description = "HTTP port";
    };

    hosts = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule {
        options = {
          hostname = lib.mkOption {
            type = lib.types.str;
            description = "Virtual host hostname (e.g., code.example.net)";
          };
          tlsCert = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Path to TLS certificate PEM file";
          };
          tlsKey = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Path to TLS private key PEM file";
          };
        };
      });
      description = "Virtual hosts to serve. At least one required.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Open the HTTP and HTTPS ports in the firewall";
    };

    stateDirectory = lib.mkOption {
      type = lib.types.str;
      default = "meow-web";
      description = "State directory name under /var/lib";
    };

    opencodeBin = lib.mkOption {
      type = lib.types.str;
      description = ''
        Path to the opencode binary. meow spawns this as a child process
        for each project session (non-sandbox mode only).
      '';
    };

    sandbox = {
      enable = lib.mkEnableOption "nix-jail sandbox for opencode sessions";

      njBin = lib.mkOption {
        type = lib.types.str;
        default = "${monorepoPackages.nix-jail}/bin/nj";
        description = "Path to the nix-jail client binary (nj)";
      };

      aliceBin = lib.mkOption {
        type = lib.types.str;
        default = "${monorepoPackages.alice}/bin/alice";
        description = "Path to the alice MITM proxy binary";
      };

      sandboxPackage = lib.mkOption {
        type = lib.types.str;
        description = ''
          Nix flake installable providing the sandbox devShell (opencode, zsh,
          coreutils, git). E.g. "/etc/nixos#devShells.x86_64-linux.sandbox".
        '';
      };

      nixJailUrl = lib.mkOption {
        type = lib.types.str;
        default = "http://127.0.0.1:50051";
        description = ''
          gRPC URL of the nixjaild daemon used by `nj exec`.
          The daemon must be running on the same host as meow-web.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts =
      lib.mkIf cfg.openFirewall ([cfg.httpPort] ++ lib.optional (cfg.port != cfg.httpPort) cfg.port);

    users.users.meow-web = {
      isSystemUser = true;
      group = "meow-web";
      home = homeDir;
    };
    users.groups.meow-web = {};

    # Create state directory structure.
    # meow uses XDG dirs relative to $HOME:
    #   config: $HOME/.config/meow/config.yaml
    #   data:   $HOME/.local/share/meow/web.port
    systemd.tmpfiles.rules = [
      "d ${homeDir}/.config 0750 meow-web meow-web -"
      "d ${configDir} 0750 meow-web meow-web -"
      "d ${homeDir}/.local 0750 meow-web meow-web -"
      "d ${homeDir}/.local/share 0750 meow-web meow-web -"
      "d ${homeDir}/.local/share/meow 0750 meow-web meow-web -"
      # Symlink the generated config into the XDG config location
      "L+ ${configDir}/config.yaml - - - - ${configFile}"
    ];

    systemd.services.meow-web = {
      description = "meow web proxy for opencode sessions";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];

      path = [
        pkgs.git
        pkgs.direnv
      ];

      unitConfig = {
        # Don't start until the repo has been cloned manually
        ConditionPathExists = cfg.repoPath;
      };

      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/meow web";
        Restart = "on-failure";
        RestartSec = "5s";
        User = "meow-web";
        Group = "meow-web";
        StateDirectory = cfg.stateDirectory;
        StateDirectoryMode = "0750";
        Environment = [
          "HOME=${homeDir}"
          "PATH=${cfg.opencodeBin}:${lib.makeBinPath [pkgs.git pkgs.direnv]}:/run/current-system/sw/bin"
        ];

      };
    };
  };
}

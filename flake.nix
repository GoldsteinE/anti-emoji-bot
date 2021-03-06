# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "Anti emoji bot";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc902;

        packageName = "anti-emoji-bot";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            text-icu = haskellPackages.text-icu_0_8_0_1;
           };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            pkg-config
            cabal-install
            zlib
            icu
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };

        nixosModules.default = with pkgs.lib; { config, ... }:
          let cfg = config.services.emojiBot;
          in   {
            options.services.emojiBot = {
              enable = mkEnableOption "emoji bot service";
              envFile = mkOption {
                type = types.str;
                default = "/etc/emoji-bot.env";
              };
            };
            config = mkIf cfg.enable {
              systemd.services.emojiBot = {
                wantedBy = [ "multi-user.target" ];
                serviceConfig.ExecStart = "${self.defaultPackage.${system}}/bin/haskell-stuff";
                serviceConfig.EnvironmentFile = cfg.envFile;
              };
            };
          };
      });
}

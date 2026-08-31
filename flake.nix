{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, lib, ... }:
        {
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc910;

            packages = {
            };
            settings = {
            };
            devShell = {
              enable = true;

              tools = hp: {
                alex = hp.alex;
                happy = hp.happy;
                fourmolu = hp.fourmolu;
                cabal-install = hp.cabal-install;
                haskell-language-server = hp.haskell-language-server;
                hlint = hp.hlint;
                ghcid = hp.ghcid;
              };

              mkShellArgs = {
                nativeBuildInputs = [
                  # helper to spin up the site locally
                  (pkgs.writeShellScriptBin "site-watch" ''
                    set -eu
                    cd corbenic-site
                    cabal build site
                    exec cabal run site -- watch
                  '')
                  pkgs.typst
                ];
              };
            };
          };

          # haskell-flake builds the Hakyll site executable automatically.
          # since it shells out to `typst`, wrap it to make typst available at runtime.
          packages.corbenic-site-wrapped =
            let
              site = pkgs.haskell.lib.justStaticExecutables self'.packages.corbenic-site;
            in
            pkgs.symlinkJoin {
              name = "corbenic-site-wrapped";
              paths = [ site ];
              buildInputs = [ pkgs.makeWrapper ];
              postBuild = ''
                wrapProgram $out/bin/site \
                  --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.typst ]}
              '';
            };

          packages.corbenic-site-dist = pkgs.runCommand "corbenic-site-dist"
            {
              nativeBuildInputs = [ self'.packages.corbenic-site-wrapped ];
              src = ./corbenic-site;
              preferLocalBuild = true;
            }
            ''
              export HAKYLL_DESTINATION=$out
              export XDG_CACHE_HOME=$(mktemp -d)
              cp -r $src ./site-src
              chmod -R u+w ./site-src
              cd ./site-src
              site build
            '';

          packages.default = self'.packages.corbenic-repl;
        };
    };
}

{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      overlays = [
        haskellNix.overlay
        (final: prev: {
          xmonad-config = final.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc925";
            shell.tools = {
              cabal = {};
              cabal-fmt = {};
              hlint = {};
              implicit-hie = {};
              haskell-language-server = {};
              fast-tags = {};
            };
            shell.buildInputs = with pkgs; [
              nixpkgs-fmt
            ];
            modules = [
              {
                packages.X11.components.library.libs = pkgs.lib.mkForce (with pkgs.xorg; [libX11 libXrandr libXext libXScrnSaver libXinerama]);
              }
            ];
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake =
        pkgs.xmonad-config.flake {
        };
    in
      flake
      // {
        packages.default = flake.packages."xmonad-config:exe:xmonad-config";
      });
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}

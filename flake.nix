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
          xmonad-config = final.haskell-nix.project' rec {
            src = ./.;
            index-state = "2023-01-21T20:07:39Z";
            compiler-nix-name = "ghc925";
            shell.tools = {
              cabal = {inherit index-state;};
              cabal-fmt = {inherit index-state;};
              hlint = {inherit index-state;};
              implicit-hie = {inherit index-state;};
              haskell-language-server = {inherit index-state;};
            };
            shell.buildInputs = with pkgs; [
              nixpkgs-fmt
            ];
            modules = [
              {
                packages.X11.components.library.libs = pkgs.lib.mkForce (with pkgs.xorg; [libX11 libXrandr libXext libXScrnSaver libXinerama]);
                packages.gi-gtk-hs.components.library = {
                  doHaddock = false;
                };
                packages.gi-gtk.components.library = {
                  doHaddock = false;
                };
                packages.taffybar.components.library = {
                  doHaddock = false;
                };
                packages.pango.components.library = {
                  doHaddock = false;
                  libs = pkgs.lib.mkForce (with pkgs; [pango glib pkg-config]);
                };
                packages.glib.components.library = {
                  doHaddock = false;
                  libs = pkgs.lib.mkForce (with pkgs; [glib pkg-config]);
                };
                packages.taffybar.components.library.libs = pkgs.lib.mkForce (with pkgs; [gtk4]);
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
        packages.xmonad-config = flake.packages."xmonad-config:exe:xmonad-config";
        packages.xmobar-config = flake.packages."xmonad-config:exe:xmobar-config";
      });
  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}

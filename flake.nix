{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        ...
      }: {
        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://haskell.flake.page/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://haskell.flake.page/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          # packages = {
          #   aeson.source = "1.5.0.0"; # Hackage version override
          #   shower.source = inputs.shower;
          # };
          settings = {
            alsa-core = {
              extraBuildDepends = with pkgs; [alsa-lib];
            };
            X11 = {
              extraBuildDepends = with pkgs.xorg; [libX11 libXrandr libXext libXScrnSaver libXinerama];
            };
            sdl2-mixer = {
              extraBuildDepends = with pkgs; [SDL2_mixer];
            };
            pango = {
              haddock = false;
              extraBuildDepends = with pkgs; [pango glib pkg-config];
            };
            glib = {
              haddock = false;
              extraBuildDepends = with pkgs; [glib pkg-config];
            };
            taffybar = {
              haddock = false;
              extraBuildDepends = with pkgs; [gtk4];
            };
          };

          devShell = {
            enable = true;

            hlsCheck.enable = true;
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.xmonad-config;
      };
    };
}

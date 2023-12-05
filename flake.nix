{
  description = "My collection of Haskell snippets for LuaSnip";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plenary-nvim = {
      url = "github:nvim-lua/plenary.nvim";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pre-commit-hooks,
    neovim-nightly-overlay,
    plenary-nvim,
    ...
  }: let
    name = "haskell-snippets.nvim";

    plugin-overlay = import ./nix/plugin-overlay.nix {
      inherit name self;
    };

    supportedSystems = [
      "aarch64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
    ];
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      ci-overlay = import ./nix/ci-overlay.nix {
        inherit
          self
          plenary-nvim
          ;
      };

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          ci-overlay
          neovim-nightly-overlay.overlay
          plugin-overlay
        ];
      };

      docgen = pkgs.callPackage ./nix/docgen.nix {};

      pre-commit-check = pre-commit-hooks.lib.${system}.run {
        src = self;
        hooks = {
          alejandra.enable = true;
          stylua.enable = true;
          luacheck.enable = true;
          editorconfig-checker.enable = true;
          markdownlint.enable = true;
        };
      };

      devShell = pkgs.mkShell {
        name = "haskell-snippets-devShell";
        inherit (pre-commit-check) shellHook;
        buildInputs = with pre-commit-hooks.packages.${system}; [
          alejandra
          lua-language-server
          stylua
          luacheck
          editorconfig-checker
          markdownlint-cli
        ];
      };
    in {
      devShells = {
        default = devShell;
        inherit devShell;
      };

      packages = rec {
        default = haskell-snippets-nvim;
        inherit docgen;
        inherit
          (pkgs)
          haskell-snippets-nvim
          ;
      };

      checks = {
        formatting = pre-commit-check;
        inherit
          (pkgs)
          nvim-stable-tests
          nvim-nightly-tests
          ;
      };
    })
    // {
      overlays.default = plugin-overlay;
    };
}

# Add flake.nix test inputs as arguments here
{
  self,
  plenary-nvim,
}: final: prev:
with final.lib;
with final.stdenv; let
  nvim-nightly = final.neovim-nightly;

  plenary-plugin = final.pkgs.vimUtils.buildVimPlugin {
    name = "plenary.nvim";
    src = plenary-nvim;
  };

  nvim-treesitter-plugin = final.pkgs.vimPlugins.nvim-treesitter.withPlugins (p: [p.haskell]);

  mkPlenaryTest = {
    name,
    nvim ? final.neovim-unwrapped,
    extraPkgs ? [],
  }: let
    nvim-wrapped = final.pkgs.wrapNeovim nvim {
      configure = {
        customRC = ''
          lua << EOF
          vim.cmd('runtime! plugin/plenary.vim')
          EOF
        '';
        packages.myVimPackage = {
          start = with final.pkgs.vimPlugins; [
            final.haskell-snippets-nvim
            plenary-plugin
            luasnip
            nvim-treesitter-plugin
          ];
        };
      };
    };
  in
    mkDerivation {
      inherit name;

      src = self;

      phases = [
        "unpackPhase"
        "buildPhase"
        "checkPhase"
      ];

      doCheck = true;

      buildInputs = with final;
        [
          nvim-wrapped
          makeWrapper
        ]
        ++ extraPkgs;

      buildPhase = ''
        mkdir -p $out
        cp -r tests $out
      '';

      checkPhase = ''
        export HOME=$(realpath .)
        export TEST_CWD=$(realpath $out/tests)
        cd $out
        nvim --headless --noplugin -c "PlenaryBustedDirectory tests {nvim_cmd = 'nvim'}"
      '';
    };
in {
  nvim-stable-tests = mkPlenaryTest {name = "neovim-stable-tests";};
  nvim-nightly-tests = mkPlenaryTest {
    name = "neovim-nightly-tests";
    nvim = nvim-nightly;
  };
}

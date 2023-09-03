{
  name,
  self,
}: final: prev: {
  haskell-snippets-nvim = final.pkgs.vimUtils.buildVimPluginFrom2Nix {
    inherit name;
    src = self;
  };
}

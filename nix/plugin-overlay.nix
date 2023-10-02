{
  name,
  self,
}: final: prev: {
  haskell-snippets-nvim = final.pkgs.vimUtils.buildVimPlugin {
    inherit name;
    src = self;
  };
}

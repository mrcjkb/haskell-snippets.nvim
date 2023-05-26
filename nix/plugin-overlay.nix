{
  name,
  self,
}: final: prev: {
  nvim-plugin = final.pkgs.vimUtils.buildVimPluginFrom2Nix {
    inherit name;
    src = self;
  };
}

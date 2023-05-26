{pkgs, ...}:
pkgs.writeShellApplication {
  name = "docgen";
  runtimeInputs = with pkgs; [
    lemmy-help
  ];
  text = ''
    mkdir -p doc
    lemmy-help lua/haskell-snippets/{init,pragmas,module,data,functions,expressions,quasiquotes}.lua > doc/haskell-snippets.txt
  '';
}

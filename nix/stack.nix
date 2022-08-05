let
  pkgs = import ((import ../default.nix).inputs.nixpkgs) { };
  compiler = pkgs.haskell.packages."ghc902";
in
pkgs.haskell.lib.buildStackProject {
  name = "env-guard";

  buildInputs = with pkgs; [
    git
    stack
    zlib.dev
    zlib.out
  ];

  ghc = compiler.ghc;

  STACK_YAML = "stack.yaml";
}
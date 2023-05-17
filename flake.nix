{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-haskell.url = "github:NixOS/nixpkgs/03fb72201639e5274fee6d77b0d9c66e98329aba";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    utils,
    ...
  }:
    utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      pkgs-haskell = import inputs.nixpkgs-haskell {inherit system;};
    in {
      devShells.default = pkgs.mkShell {
        buildInputs = (with pkgs-haskell.haskell.packages.ghc944; [
          cabal-install
          ghc
          haskell-language-server
          hp2pretty
        ]) ++ (with pkgs; [ ghcid ]);
      };
    });
}

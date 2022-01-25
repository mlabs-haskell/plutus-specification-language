{
  description = "A cardano specification language using idris2.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        p = import nixpkgs {inherit system;};
        spec-language-deriv = {
          name = "cardano-spec-language";
          src = self;
          buildInputs = [ p.idris2 ];
        };
      in {
        devShell = p.mkShell spec-language-deriv;
        defaultPackage = p.stdenv.mkDerivation (spec-language-deriv // {
          installPhase = "idris2  --build ./psl.ipkg  --build-dir $out";
        });
      });
}

{
  description = "PSL";
  inputs = {
    plutarch.url = "github:Plutonomicon/plutarch-core";
    plutarch.flake = false;
    plutarch.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  };
  outputs = { self, plutarch, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};
      hsOverlay = hsPkgs: hsPkgs.override {
        overrides = final: prev: {
          plutarch-core = final.callPackage "${plutarch}/plutarch-core.nix" {};
          psl = final.callPackage ./psl.nix {};
        };
      };
      hsPkgsFor = system: hsOverlay (pkgsFor system).haskell.packages.ghc923;
    in
    {
      checks = perSystem (system: {
        formatting = (pkgsFor system).runCommandNoCC "formatting-check" {} ''
          cd ${self}
          ./bin/format check
          touch $out
        '';
        cabal2nix = (pkgsFor system).runCommandNoCC "cabal2nix-check" {
          nativeBuildInputs = [ (pkgsFor system).cabal2nix ];
        } ''
          cd ${self}
          diff <(cabal2nix ./.) psl.nix
          touch $out
        '';
      });
      apps = perSystem (system: {
        regen.type = "app";
        regen.program = builtins.toString ((pkgsFor system).writeShellScript "regen" ''
          set -xe
          ${(pkgsFor system).cabal2nix}/bin/cabal2nix ./. > psl.nix
          ./bin/format
        '');
      });
      packages = perSystem (system: {
        default = (hsPkgsFor system).psl;
      });
      devShells = perSystem (system: {
        default = (hsPkgsFor system).shellFor {
          packages = p: [ p.psl ];
          buildHoogle = true;
          nativeBuildInputs = with (pkgsFor system); [
            cabal-install
            hlint
            cabal2nix
            nixpkgs-fmt
            curl
            haskellPackages.cabal-fmt
            (hsPkgsFor system).fourmolu_0_7_0_1
          ];
        };
      });
    };
}

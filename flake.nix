{
  description = "Plutus specification language";

  inputs.idris = {
    url = "github:idris-lang/Idris2";
  };
  inputs.nixpkgs.follows = "idris/nixpkgs";
  inputs.flake-utils.follows = "idris/flake-utils";

  outputs = { self, nixpkgs, idris, flake-utils }: flake-utils.lib.eachDefaultSystem
    (system:
      let pkgs = nixpkgs.legacyPackages.${system}; in
      rec {
        packages.default = (idris.buildIdris.${system} {
          projectName = "psl";
          src = "${self}";
          idrisLibraries = [ ];
        }).installLibrary;
        devShells.default = pkgs.mkShell {
          buildInputs = [ idris.packages.${system}.idris2 pkgs.rlwrap ];
          shellHook = ''
            alias idris2="rlwrap -s 1000 idris2 --no-banner"
          '';
        };
      }
    ) // {
    herculesCI.ciSystems = [ "x86_64-linux" ];
  };
}

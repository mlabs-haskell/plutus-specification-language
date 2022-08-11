{ mkDerivation, base, lib, plutarch-core }:
mkDerivation {
  pname = "psl";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base plutarch-core ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}

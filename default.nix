with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    bimaps = ../Lib-bimaps;
    BiobaseTypes = ./.;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseTypes ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      bimaps
      DPutils
      ForestStructures
      OrderedBits
      PrimitiveArray
      SciBaseTypes
    ];
  };
}

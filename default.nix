with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-bimaps
    ../Lib-ForestStructures
    ../Lib-PrimitiveArray
    ../Lib-SciBaseTypes
  ]) // {BiobaseTypes = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseTypes ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      bimaps
      ForestStructures
      PrimitiveArray
      SciBaseTypes
    ];
  };
}

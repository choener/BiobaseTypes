{ mkDerivation, aeson, attoparsec, base, bimaps, binary, bytestring
, cereal, cereal-text, cereal-vector, containers, data-default
, deepseq, ForestStructures, hashable, intern, lens, mtl, primitive
, PrimitiveArray, QuickCheck, SciBaseTypes, stdenv
, string-conversions, tasty, tasty-quickcheck, tasty-th, text
, text-binary, utf8-string, vector, vector-binary-instances
, vector-th-unbox
}:
mkDerivation {
  pname = "BiobaseTypes";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bimaps binary bytestring cereal cereal-text
    cereal-vector containers data-default deepseq ForestStructures
    hashable intern lens mtl primitive PrimitiveArray QuickCheck
    SciBaseTypes string-conversions text text-binary utf8-string vector
    vector-binary-instances vector-th-unbox
  ];
  testHaskellDepends = [
    base bytestring lens QuickCheck tasty tasty-quickcheck tasty-th
  ];
  homepage = "https://github.com/choener/BiobaseTypes";
  description = "Collection of types for bioinformatics";
  license = stdenv.lib.licenses.bsd3;
}

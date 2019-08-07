{ mkDerivation, aeson, attoparsec, base, bimaps, binary, bytestring
, cereal, cereal-text, cereal-vector, containers, data-default
, deepseq, ForestStructures, hashable, intern, lens, mtl, primitive
, PrimitiveArray, QuickCheck, SciBaseTypes, stdenv, streaming
, string-conversions, tasty, tasty-hunit, tasty-quickcheck
, tasty-th, text, text-binary, utf8-string, vector
, vector-binary-instances, vector-th-unbox
}:
mkDerivation {
  pname = "BiobaseTypes";
  version = "0.2.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bimaps binary bytestring cereal cereal-text
    cereal-vector containers data-default deepseq ForestStructures
    hashable intern lens mtl primitive PrimitiveArray QuickCheck
    SciBaseTypes streaming string-conversions text text-binary
    utf8-string vector vector-binary-instances vector-th-unbox
  ];
  testHaskellDepends = [
    aeson attoparsec base bimaps binary bytestring cereal cereal-text
    cereal-vector containers data-default deepseq ForestStructures
    hashable intern lens mtl primitive PrimitiveArray QuickCheck
    SciBaseTypes streaming string-conversions tasty tasty-hunit
    tasty-quickcheck tasty-th text text-binary utf8-string vector
    vector-binary-instances vector-th-unbox
  ];
  homepage = "https://github.com/choener/BiobaseTypes";
  description = "Collection of types for bioinformatics";
  license = stdenv.lib.licenses.bsd3;
}

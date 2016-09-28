
-- | Names for biological things.
--
-- Species names are internalized and represented as an @Int@. This allows
-- using them in structures like an @IntMap@.
--
-- For other names, we newtype-wrap normal text internalization.
--

module Biobase.Types.Names where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Data.Aeson as A
import Data.Binary      as DB
import Data.Hashable
import Data.Interned
import Data.Interned.Text
import Data.Serialize   as DS
import Data.Serialize.Text
import Data.String as IS
import Data.String.Conversions (ConvertibleStrings(..), cs)
import Data.String.Conversions.Monomorphic (toST, fromST)
import Data.Text.Binary
import Data.Text (Text, pack, unpack)
import Data.Vector.Unboxed.Deriving
import GHC.Generics

import Biobase.Types.Names.Internal



-- * Int-internalized species names.

-- | A species name. Represented with an @Int@, but behaves like a @Text@.

newtype SpeciesName = SpeciesName { getSpeciesNameRep :: Int }
  deriving (Eq,Generic)

derivingUnbox "SpeciesName"
  [t| SpeciesName -> Int |]
  [|  getSpeciesNameRep  |]
  [|  SpeciesName        |]

instance Ord SpeciesName where
  SpeciesName l `compare` SpeciesName r = speciesNameBimapLookupInt l `compare` speciesNameBimapLookupInt r
  {-# Inline compare #-}

-- | Smart constructor that performs the correct internalization.

speciesName :: Text -> SpeciesName
speciesName = SpeciesName . speciesNameBimapAdd
{-# Inline speciesName #-}

instance IsString SpeciesName where
  fromString = speciesName . IS.fromString
  {-# Inline fromString #-}

instance Show SpeciesName where
  showsPrec p i r = showsPrec p (unpack $ toST i) r
  {-# Inline showsPrec #-}

instance Read SpeciesName where
  readsPrec p str = [ (speciesName $ IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable SpeciesName

instance ConvertibleStrings Text SpeciesName where
  convertString = speciesName

instance ConvertibleStrings SpeciesName Text where
  convertString = speciesNameBimapLookupInt . getSpeciesNameRep

instance NFData SpeciesName

instance Binary SpeciesName where
  put = DB.put . toST
  get = fromST <$> DB.get
  {-# Inline put #-}
  {-# Inline get #-}

instance Serialize SpeciesName where
  put = DS.put . toST
  get = fromST <$> DS.get
  {-# Inline put #-}
  {-# Inline get #-}

instance FromJSON SpeciesName where
  parseJSON s = fromST <$> parseJSON s
  {-# Inline parseJSON #-}

instance ToJSON SpeciesName where
  toJSON = toJSON . toST
  {-# Inline toJSON #-}



-- * Internalize taxonomic rank names

-- | The taxonomic rank. This encodes the name for a given rank.

newtype TaxonomicRank = TaxonomicRank { getTaxonomicRank :: InternedText }
  deriving (IsString,Eq,Ord,Show,Generic)

instance NFData TaxonomicRank where
  rnf (TaxonomicRank it) = rnf (internedTextId it)
  {-# Inline rnf #-}

instance ConvertibleStrings Text TaxonomicRank where
  convertString = TaxonomicRank . intern

instance ConvertibleStrings TaxonomicRank Text where
  convertString = unintern . getTaxonomicRank

instance Hashable TaxonomicRank where
  hashWithSalt s (TaxonomicRank it) = hashWithSalt s (internedTextId it)
  {-# Inline hashWithSalt #-}

instance Read TaxonomicRank where
  readsPrec p str = [ (IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Binary TaxonomicRank where
  put = DB.put . toST
  get = fromST <$> DB.get
  {-# Inline put #-}
  {-# Inline get #-}

instance Serialize TaxonomicRank where
  put = DS.put . toST
  get = fromST <$> DS.get
  {-# Inline put #-}
  {-# Inline get #-}

instance FromJSON TaxonomicRank where
  parseJSON s = fromST <$> parseJSON s
  {-# Inline parseJSON #-}

instance ToJSON TaxonomicRank where
  toJSON = toJSON . toST
  {-# Inline toJSON #-}


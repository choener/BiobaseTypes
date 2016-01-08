
-- | Names for biological things.
--
-- TODO where useful, we should use internalization. One big point are
-- species names. With certain problems, like MAF file reading, there is *a
-- lot* of redundancy.

module Biobase.Types.Names where

import           Control.Applicative
import           Control.DeepSeq (NFData(..))
import           Data.Aeson as A
import           Data.Binary      as DB
import           Data.Hashable
import           Data.Serialize   as DS
import           Data.Serialize.Text
import           Data.Stringable as SA
import           Data.String as IS
import           Data.Text.Binary
import           Data.Text (Text)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics

import           Biobase.Types.Names.Internal



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

speciesName :: Text -> SpeciesName
speciesName = SpeciesName . speciesNameBimapAdd
{-# Inline speciesName #-}

instance IsString SpeciesName where
  fromString = speciesName . IS.fromString
  {-# Inline fromString #-}

instance Show SpeciesName where
  showsPrec p i r = showsPrec p (toString i) r
  {-# Inline showsPrec #-}

instance Read SpeciesName where
  readsPrec p str = [ (speciesName $ IS.fromString s, y) | (s,y) <- readsPrec p str ]
  {-# Inline readsPrec #-}

instance Hashable SpeciesName

instance Stringable SpeciesName where
  toString    = toString . speciesNameBimapLookupInt . getSpeciesNameRep
  fromString  = speciesName . SA.fromString
  length      = SA.length . speciesNameBimapLookupInt . getSpeciesNameRep
  toText      = toText . speciesNameBimapLookupInt . getSpeciesNameRep
  fromText    = speciesName . fromText
  {-# Inline toString   #-}
  {-# Inline fromString #-}
  {-# Inline length     #-}
  {-# Inline toText     #-}
  {-# Inline fromText   #-}

instance NFData SpeciesName

instance Binary SpeciesName where
  put = DB.put . toText
  get = fromText <$> DB.get
  {-# Inline put #-}
  {-# Inline get #-}

instance Serialize SpeciesName where
  put = DS.put . toText
  get = fromText <$> DS.get
  {-# Inline put #-}
  {-# Inline get #-}

instance FromJSON SpeciesName where
  parseJSON s = fromText <$> parseJSON s
  {-# Inline parseJSON #-}

instance ToJSON SpeciesName where
  toJSON = toJSON . toText
  {-# Inline toJSON #-}


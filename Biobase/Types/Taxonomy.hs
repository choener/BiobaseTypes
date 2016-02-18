
-- | Biological classification of species.

module Biobase.Types.Taxonomy where

import Control.DeepSeq
import Data.Aeson
import Data.Binary
import Data.Hashable (Hashable, hashWithSalt)
import Data.Primitive.Types
import Data.Serialize
import Data.Text (Text)
import Data.Vector.Binary
import Data.Vector.Serialize
import Data.Vector.Unboxed.Deriving
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Biobase.Types.Accession (Accession,Species)
import Biobase.Types.Names (SpeciesName, TaxonomicRank)



-- | Taxonomic classification. @Enum@ together with a final @Unknown@ is
-- somewhat fishy.

data Classification
  = Kingdom
  | Phylum
  | Class
  | Order
  | SubOrder
  | Family
  | Genus
  | Species
  | Unknown
  deriving (Eq,Ord,Read,Show,Enum,Generic)

instance Binary    Classification
instance FromJSON  Classification
instance Hashable  Classification
instance Serialize Classification
instance ToJSON    Classification
instance NFData    Classification

derivingUnbox "Classification"
  [t| Classification -> Int |] [| fromEnum |] [| toEnum |]



-- | A somewhat generic representation of a species within a taxonomic
-- context.

data Taxon = Taxon
  { species         :: !SpeciesName                             -- ^ the full, formal name of a species
  , accession       :: !(Accession Species)                     -- ^ the accession for the species (or @""@ if unknown)
  , classification  :: !(Vector (TaxonomicRank,Classification)) -- ^ vector with classification information
  }
  deriving (Eq,Read,Show,Generic)

instance Binary    Taxon
instance FromJSON  Taxon
instance Serialize Taxon
instance ToJSON    Taxon
instance NFData    Taxon

instance Hashable Taxon where
  hashWithSalt h (Taxon s a _) = hashWithSalt h (s,a)


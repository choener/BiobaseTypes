
module Biobase.Types.Accession where

import Data.Aeson
import Data.Binary
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Serialize
import Data.Serialize.Text
import Data.String
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Stringable



-- * 'Accession' with phantom types.

-- | The accession number is a unique identifier in bioinformatics.
--
-- Depending on the source, accession numbers follow different alphanumeric
-- formats! While letters-than-numbers is quite common, swissprot uses
-- a mix. Hence, we just use a text string as accession.
--
-- A phantom type is provided to enable type safety annotations. Helper
-- functions provide smart construction from the @Accession@ tagged generic
-- type.

newtype Accession t = Accession { _getAccession :: Text }
  deriving (Eq,Ord,Read,Show,Generic)

accession :: Stringable s => s -> Accession t
accession = Accession . toText
{-# Inline accession #-}

tagAccession :: Accession f -> Accession t
tagAccession = Accession . _getAccession
{-# Inline tagAccession #-}

instance IsString (Accession t) where
  fromString = accession
  {-# Inline fromString #-}

instance Binary    (Accession t)
instance FromJSON  (Accession t)
instance Hashable  (Accession t)
instance Serialize (Accession t)
instance ToJSON    (Accession t)

-- ** NCBI

data Nucleotide   -- ^ nucleotide sequence
data Protein      -- ^ protin sequence

-- ** Rfam phantom types
--
-- The format is RFxxxxx, PFxxxxx, or CLxxxxx.

data Clan     -- ^ Tag as being a clan.
data Pfam     -- ^ Tag as being a Pfam model.
data Rfam     -- ^ Tag as being an Rfam model. Used for Stockholm and CM files.


data Species  -- ^ Species have an accession number, too.


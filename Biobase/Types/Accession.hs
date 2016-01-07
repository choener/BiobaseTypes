
-- | Accession numbers. These /numbers/ are not really numbers because they
-- they are made up of alphanumeric characters.

module Biobase.Types.Accession where

import Control.DeepSeq
import Data.Aeson
import Data.Binary
import Data.Hashable (Hashable)
import Data.Ix (Ix)
import Data.Serialize
import Data.Serialize.Text
import Data.String
import Data.Stringable
import Data.Text.Binary
import Data.Text (Text, span)
import Data.Char (isLetter)
import GHC.Generics (Generic)
import Prelude hiding (length,span)



-- * 'Accession' with phantom types.
--
-- <http://www.ncbi.nlm.nih.gov/Sequin/acc.html>
--
-- <http://www.uniprot.org/help/accession_numbers>
--
-- <http://en.wikipedia.org/wiki/Accession_number_%28bioinformatics%29>

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

-- | Generate an accession with an explicit phantom type: @accession'
-- Nucleotide "Bla"@ has type @:: Accession Nucleotide@.

accession' :: Stringable s => t -> s -> Accession t
accession' t = Accession . toText

-- | Generate an accession when the type @Accession t@ is clear from the
-- context.

accession :: Stringable s => s -> Accession t
accession = Accession . toText
{-# Inline accession #-}

-- | Retag an accession

retagAccession :: Accession f -> Accession t
retagAccession = Accession . _getAccession
{-# Inline retagAccession #-}

instance IsString (Accession t) where
  fromString = accession
  {-# Inline fromString #-}

instance Binary    (Accession t)
instance FromJSON  (Accession t)
instance Hashable  (Accession t)
instance Serialize (Accession t)
instance ToJSON    (Accession t)
instance NFData    (Accession t)



-- * Phantom types. All with an excliti data constructor to guide
-- 'accession''.

-- ** NCBI phantom types

-- | nucleotide sequence

data Nucleotide = Nucleotide

-- | protein sequence

data Protein = Protein

-- ** Rfam phantom types
--
-- The format is RFxxxxx, PFxxxxx, or CLxxxxx.

-- | Tag as being a clan.

data Clan = Clan

-- | Tag as being a Pfam model.

data Pfam = Pfam

-- | Tag as being an Rfam model. Used for Stockholm and CM files.

data Rfam = Rfam

-- | Species have an accession number, too.

data Species = Species



-- * Helper functions

-- | Guess the type of accession number. Returns @Nothing@ if unknown
-- structure.

guessAccessionType :: Accession t -> Maybe Text
guessAccessionType (Accession a) = case (length l, length d) of
  (1,5)                   -> Just "Nucleotide"
  (2,6)                   -> Just "Nucleotide"
  (3,5)                   -> Just "Protein"
  (3,k) | 8<= k && k<= 10 -> Just "WGS"
  (5,7)                   -> Just "MGA"
  _                       -> Nothing
  where (l,d) = span isLetter a



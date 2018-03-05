
-- | Wrappers around biosequences.

module Biobase.Types.NucleotideSequence where

import           Control.DeepSeq
import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Char (ord,chr,toUpper)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU
import           Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as TQ



-- | A sequence identifier. Just a newtype wrapped text field. Because we can
-- never know what people are up to, this is utf8-encoded.
--
-- TODO Provide @Iso'@ for @Text@, too?
--
-- TODO move into @Biobase.Types.SequenceID@

newtype SequenceID = SequenceID { _sequenceID ∷ ByteString }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show, IsString)
makeLenses ''SequenceID

instance NFData SequenceID

-- | Convert to a string in a unicode-aware manner.

sequenceIDstring ∷ Iso' SequenceID String
sequenceIDstring = sequenceID . iso BSU.toString BSU.fromString
{-# Inline sequenceIDstring #-}



-- | A short RNA sequence.
--
-- It is an instance of 'Ixed' to allow @RNAseq (BS.pack "cag") ^? ix 2 == Just 'g'@.

newtype RNAseq = RNAseq { _rnaseq ∷ ByteString }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''RNAseq

instance NFData RNAseq

type instance Index RNAseq = Int

type instance IxValue RNAseq = Char

instance Ixed RNAseq where
  ix k = rnaseq . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving instance Reversing RNAseq

mkRNAseq ∷ ByteString → RNAseq
mkRNAseq = RNAseq . BS.map go . BS.map toUpper
  where go x | x `elem` acgu = x
             | otherwise     = 'N'
        acgu ∷ String
        acgu = "ACGU"

instance IsString RNAseq where
  fromString = mkRNAseq . BS.pack

instance Arbitrary RNAseq where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ACGU"
    return . RNAseq $ BS.pack xs
  shrink = view (to shrink)



-- | A short DNA sequence.
--
-- Note everything really long should be handled by specialized libraries with
-- streaming capabilities.

newtype DNAseq = DNAseq { _dnaseq ∷ ByteString }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''DNAseq

instance NFData DNAseq

type instance Index DNAseq = Int

type instance IxValue DNAseq = Char

instance Ixed DNAseq where
  ix k = dnaseq . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

mkDNAseq ∷ ByteString → DNAseq
mkDNAseq = DNAseq . BS.map go . BS.map toUpper
  where go x | x `elem` acgt = x
             | otherwise     = 'N'
        acgt ∷ String
        acgt = "ACGT"

instance IsString DNAseq where
  fromString = mkDNAseq . BS.pack

deriving instance Reversing DNAseq

instance Arbitrary DNAseq where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ACGT"
    return . DNAseq $ BS.pack xs
  shrink = view (to shrink)

-- | Simple case translation from @U@ to @T@. with upper and lower-case
-- awareness.

rna2dna ∷ Char → Char
rna2dna = \case
  'U' → 'T'
  'u' → 't'
  x   → x
{-# Inline rna2dna #-}

-- | Single character RNA complement.

rnaComplement ∷ Char → Char
rnaComplement = \case
  'A' → 'U'
  'a' → 'u'
  'C' → 'G'
  'c' → 'g'
  'G' → 'C'
  'g' → 'c'
  'U' → 'A'
  'u' → 'a'
  x   → x
{-# Inline rnaComplement #-}

-- | Simple case translation from @T@ to @U@ with upper- and lower-case
-- awareness.

dna2rna ∷ Char → Char
dna2rna = \case
  'T' → 'U'
  't' → 'u'
  x   → x
{-# Inline dna2rna #-}

-- | Single character DNA complement.

dnaComplement ∷ Char → Char
dnaComplement = \case
  'A' → 'T'
  'a' → 't'
  'C' → 'G'
  'c' → 'g'
  'G' → 'C'
  'g' → 'c'
  'T' → 'A'
  't' → 'a'
  x   → x
{-# Inline dnaComplement #-}



-- | Transcribes a DNA sequence into an RNA sequence. Note that 'transcribe' is
-- actually very generic. We just define its semantics to be that of
-- biomolecular transcription.
--
-- 'transcribe' makes the assumption that, given @DNA -> RNA@, we transcribe
-- the coding strand.
-- <http://hyperphysics.phy-astr.gsu.edu/hbase/Organic/transcription.html>
--
-- @@ DNAseq "ACGT" ^. transcribe == RNAseq "ACGU" RNAseq "ACGU" ^. transcribe
-- == DNAseq "ACGT" RNAseq "ACGU" ^. from transcribe :: DNAseq == DNAseq "ACGT"
-- @@

class Transcribe f where
  type TranscribeTo f ∷ *
  transcribe ∷ Iso' f (TranscribeTo f)

-- | Transcribe a DNA sequence into an RNA sequence. This does not @reverse@
-- the sequence!

instance Transcribe DNAseq where
  type TranscribeTo DNAseq = RNAseq
  transcribe = iso (RNAseq . BS.map dna2rna . _dnaseq) (DNAseq . BS.map rna2dna . _rnaseq)
  {-# Inline transcribe #-}

-- | Transcribe a RNA sequence into an DNA sequence. This does not @reverse@
-- the sequence!

instance Transcribe RNAseq where
  type TranscribeTo RNAseq = DNAseq
  transcribe = from transcribe
  {-# Inline transcribe #-}



-- | The complement of a biosequence.

class Complement f where
  complement ∷ Iso' f f

instance Complement DNAseq where
  complement = iso (DNAseq . BS.map dnaComplement . _dnaseq) (DNAseq . BS.map dnaComplement . _dnaseq)
  {-# Inline complement #-}

instance Complement RNAseq where
  complement = iso (RNAseq . BS.map rnaComplement . _rnaseq) (RNAseq . BS.map rnaComplement . _rnaseq)
  {-# Inline complement #-}


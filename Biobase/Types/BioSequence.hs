
-- | Abstraction over bio sequences encoded as one-ascii character as one
-- symbol. We phantom-type the exact bio-sequence type and provide type classes
-- that act on known types.
--
-- Unknown bio sequences should be tagged with @Void@.

module Biobase.Types.BioSequence where

import           Control.DeepSeq
import           Control.Lens
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (ord,chr,toUpper)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           Data.Void
import           GHC.Exts (IsString(..))
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Test.QuickCheck as TQ
import           Test.QuickCheck (Arbitrary(..))



data RNA

data DNA

data AA



newtype BioSequence (which ∷ k) = BioSequence {_bioSequence ∷ ByteString}
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeWrapped ''BioSequence
makePrisms ''BioSequence

instance NFData (BioSequence w)

type instance Index (BioSequence w) = Int

type instance IxValue (BioSequence w) = Char

instance Ixed (BioSequence w) where
  ix k = _BioSequence . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving instance Reversing (BioSequence w)

instance IsString (BioSequence Void) where
  fromString = BioSequence . BS.pack



-- * RNA

mkRNAseq ∷ ByteString → BioSequence RNA
mkRNAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` acgu = x
             | otherwise     = 'N'
        acgu ∷ String
        acgu = "ACGU"

instance IsString (BioSequence RNA) where
  fromString = mkRNAseq . BS.pack

instance Arbitrary (BioSequence RNA) where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ACGU"
    return . BioSequence $ BS.pack xs
  shrink = view (to shrink)



-- * DNA

mkDNAseq ∷ ByteString → (BioSequence DNA)
mkDNAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` acgt = x
             | otherwise     = 'N'
        acgt ∷ String
        acgt = "ACGT"

instance IsString (BioSequence DNA) where
  fromString = mkDNAseq . BS.pack

instance Arbitrary (BioSequence DNA) where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ACGT"
    return . BioSequence $ BS.pack xs
  shrink = view (to shrink)



-- * Amino acid sequences

mkAAseq ∷ ByteString → (BioSequence AA)
mkAAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` aas = x
             | otherwise    = 'X'
        aas ∷ String
        aas = "ARNDCEQGHILKMFPSTWYVUO"

instance IsString (BioSequence AA) where
  fromString = mkAAseq . BS.pack

instance Arbitrary (BioSequence AA) where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ARNDCEQGHILKMFPSTWYVUO"
    return . BioSequence $ BS.pack xs
  shrink = view (to shrink)




-- * DNA/RNA

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

instance Transcribe (BioSequence DNA) where
  type TranscribeTo (BioSequence DNA) = (BioSequence RNA)
  transcribe = iso (over _BioSequence (BS.map dna2rna)) (over _BioSequence (BS.map rna2dna))
  {-# Inline transcribe #-}

-- | Transcribe a RNA sequence into an DNA sequence. This does not @reverse@
-- the sequence!

instance Transcribe (BioSequence RNA) where
  type TranscribeTo (BioSequence RNA) = (BioSequence DNA)
  transcribe = from transcribe
  {-# Inline transcribe #-}



-- | The complement of a biosequence.

class Complement f where
  complement ∷ Iso' f f

instance Complement (BioSequence DNA) where
  complement = iso (over _BioSequence (BS.map dnaComplement)) (over _BioSequence (BS.map dnaComplement))
  {-# Inline complement #-}

instance Complement (BioSequence RNA) where
  complement = iso (over _BioSequence (BS.map rnaComplement)) (over _BioSequence (BS.map rnaComplement))
  {-# Inline complement #-}


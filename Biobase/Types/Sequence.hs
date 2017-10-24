
-- | Wrappers around biosequences.

module Biobase.Types.Sequence where

import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.Char (ord,chr)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BSU



-- | A sequence identifier. Just a newtype wrapped text field. Because we can
-- never know what people are up to, this is utf8-encoded.
--
-- TODO Provide @Iso'@ for @Text@, too?

newtype SequenceID = SequenceID { _sequenceID ∷ ByteString }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''SequenceID

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

type instance Index RNAseq = Int

type instance IxValue RNAseq = Char

instance Ixed RNAseq where
  ix k = rnaseq . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving instance Reversing RNAseq


-- | A short DNA sequence.
--
-- Note everything really long should be handled by specialized libraries with
-- streaming capabilities.

newtype DNAseq = DNAseq { _dnaseq ∷ ByteString }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''DNAseq

type instance Index DNAseq = Int

type instance IxValue DNAseq = Char

instance Ixed DNAseq where
  ix k = dnaseq . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving instance Reversing DNAseq

-- | Simple case translation from @U@ to @T@. with upper and lower-case
-- awareness.

rna2dna ∷ Char → Char
rna2dna = \case
  'U' → 'T'
  'u' → 't'
  x   → x
{-# Inline rna2dna #-}

-- | Simple case translation from @T@ to @U@ with upper- and lower-case
-- awareness.

dna2rna ∷ Char → Char
dna2rna = \case
  'T' → 'U'
  't' → 'u'
  x   → x
{-# Inline dna2rna #-}

-- | Transcribes a DNA sequence into an RNA sequence. Note that 'transcribe' is
-- actually very generic. We just define its semantics to be that of
-- biomolecular transcription.
--
-- @@
-- DNAseq "ACGT" ^. transcribe == RNAseq "ACGU"
-- RNAseq "ACGU" ^. transcribe == DNAseq "ACGT"
-- RNAseq "ACGU" ^. from transcribe :: DNAseq == DNAseq "ACGT"
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



-- | Abstraction over bio sequences encoded as one-ascii character as one
-- symbol. We phantom-type the exact bio-sequence type and provide type classes
-- that act on known types.
--
-- Unknown bio sequences should be tagged with @Void@.
--
-- TODO give (lens) usage examples

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
import qualified Data.ByteString.UTF8 as BSU
import qualified Streaming.Prelude as SP
import qualified Streaming as S
import qualified Test.QuickCheck as TQ
import           Test.QuickCheck (Arbitrary(..))

import           Biobase.Types.Location
import           Biobase.Types.Strand
import qualified Biobase.Types.Index as BTI



-- * Sequence identifiers

newtype SequenceIdentifier (which :: k) = SequenceIdentifier { _sequenceIdentifier :: ByteString }
  deriving stock (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeWrapped ''SequenceIdentifier
makePrisms ''SequenceIdentifier

instance NFData (SequenceIdentifier w)

instance IsString (SequenceIdentifier w) where
  fromString = SequenceIdentifier . BSU.fromString



-- * Bio-Sequences

data RNA

data DNA

data XNA

data AA



newtype BioSequence (which :: k) = BioSequence {_bioSequence :: ByteString}
  deriving stock (Data, Typeable, Generic, Eq, Ord, Read, Show)
  deriving newtype (Semigroup)
makeWrapped ''BioSequence
makePrisms ''BioSequence

instance NFData (BioSequence w)

type instance Index (BioSequence w) = Int

type instance IxValue (BioSequence w) = Char

instance Ixed (BioSequence w) where
  ix k = _BioSequence . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving newtype instance Reversing (BioSequence w)

instance IsString (BioSequence Void) where
  fromString = BioSequence . BS.pack



-- * RNA

-- |
--
-- TODO write that converts explicitly

mkRNAseq :: ByteString -> BioSequence RNA
mkRNAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` acgu = x
             | otherwise     = 'N'
        acgu :: String
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

mkDNAseq :: ByteString -> (BioSequence DNA)
mkDNAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` acgt = x
             | otherwise     = 'N'
        acgt :: String
        acgt = "ACGT"

instance IsString (BioSequence DNA) where
  fromString = mkDNAseq . BS.pack

instance Arbitrary (BioSequence DNA) where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ACGT"
    return . BioSequence $ BS.pack xs
  shrink = view (to shrink)



-- * XNA

mkXNAseq :: ByteString -> (BioSequence XNA)
mkXNAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` acgtu = x
             | otherwise      = 'N'
        acgtu :: String
        acgtu = "ACGTU"

instance IsString (BioSequence XNA) where
  fromString = mkXNAseq . BS.pack

instance Arbitrary (BioSequence XNA) where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ACGTU"
    return . BioSequence $ BS.pack xs
  shrink = view (to shrink)



-- * Amino acid sequences

mkAAseq :: ByteString -> (BioSequence AA)
mkAAseq = BioSequence . BS.map go . BS.map toUpper
  where go x | x `elem` aas = x
             | otherwise    = 'X'
        aas :: String
        aas = "ARNDCEQGHILKMFPSTWYVUO"

instance IsString (BioSequence AA) where
  fromString = mkAAseq . BS.pack

instance Arbitrary (BioSequence AA) where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ARNDCEQGHILKMFPSTWYVUO"
    return . BioSequence $ BS.pack xs
  shrink = view (to shrink)



-- * A window into a longer sequence with prefix/suffix information.

-- | Phantom-typed over two types, the type @w@ of the identifier, which can be
-- descriptive ("FirstInput") and the second type, identifying what kind of
-- sequence types we are dealing with. Finally, the third type provides
-- location information and should be location or streamed location.

data BioSequenceWindow w ty loc = BioSequenceWindow
  { _bswIdentifier :: !(SequenceIdentifier w)
    -- ^ Identifier for this window. Typically some fasta identifier
  , _bswPrefixLen  :: !Int
    -- ^ Any prefix for this sequence
  , _bswSequence   :: !(BioSequence ty)
    -- ^ The actual sequence, possibly with prefix of suffix attached
  , _bswSuffixLen  :: !Int
    -- ^ any suffix (length)
  , _bswLocation   :: !loc
    -- ^ full location information. This should include the prefix and suffix,
    -- if any.
  }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''BioSequenceWindow

instance NFData loc => NFData (BioSequenceWindow w ty loc)

instance (Reversing loc) => Reversing (BioSequenceWindow w ty loc) where
  {-# Inlinable reversing #-}
  reversing bsw = bsw
                & bswPrefixLen .~ (bsw^.bswSuffixLen)
                & bswSuffixLen .~ (bsw^.bswPrefixLen)
                & bswSequence .~ (bsw^.bswSequence.reversed)
                & bswLocation .~ (bsw^.bswLocation.reversed)

-- | Take only @k@ characters from a window, correctly taking into account the
-- pfx-seq-sfx, and loc information.

bswTake :: Int -> BioSequenceWindow w ty FwdLocation -> BioSequenceWindow w ty FwdLocation
{-# Inlinable bswTake #-}
bswTake k' bsw
  = over bswPrefixLen (\l -> min l k)
  . over (bswSequence._BioSequence) (BS.take k)
  . over bswSuffixLen (\l -> max 0 $ k-len+slen)
  . over bswLocation (fwdLocationTake k) $ bsw
  where plen = bsw^.bswPrefixLen; len = bsw^.bswSequence._BioSequence.to BS.length
        slen = bsw^.bswSuffixLen
        k = max 0 $ min k' len

bswDrop :: Int -> BioSequenceWindow w ty FwdLocation -> BioSequenceWindow w ty FwdLocation
{-# Inlinable bswDrop #-}
bswDrop k' bsw
  = over bswPrefixLen (\l -> max 0 $ plen-k)
  . over (bswSequence._BioSequence) (BS.drop k)
  . over bswSuffixLen (\l -> l - (max 0 $ k-len+slen))
  . over bswLocation (fwdLocationDrop k) $ bsw
  where plen = bsw^.bswPrefixLen; len = bsw^.bswSequence._BioSequence.to BS.length
        slen = bsw^.bswSuffixLen
        k = max 0 $ min k' len


-- | For each element, attach the prefix as well. This modifies the the location info!
--
-- @1 2 3 4@ -> @01 12 23 34@
--
-- TODO are we sure this is correct for @MinusStrand@?

attachPrefixes
  :: forall m w ty r
  . (Monad m) => SP.Stream (SP.Of (BioSequenceWindow w ty FwdLocation)) m r -> SP.Stream (SP.Of (BioSequenceWindow w ty FwdLocation)) m r
{-# Inlinable attachPrefixes #-}
attachPrefixes  =
  let
    f :: (BioSequenceWindow w ty FwdLocation) -> (BioSequenceWindow w ty FwdLocation) -> BioSequenceWindow w ty FwdLocation
    f pfx =
      let plen = pfx^.bswSequence._BioSequence.to BS.length
      in  set bswPrefixLen plen
          . over bswSequence (pfx^.bswSequence <>)
          . over bswLocation (pfx^.bswLocation <>)
    -- the go function just attaches prefixes.
    go (Left _empty) = Right
    go (Right p)     = Right . f p
  in  SP.map (\(Right w) -> w) . SP.drop 1 . SP.scan go (Left $ BioSequence "") id

-- | For each element, attach the suffix as well.
--
-- @1 2 3 4@ -> @12 23 34 40@

--attachSuffixes :: (Monad m) => SP.Stream (SP.Of (BioSequenceWindow w ty k)) m r -> SP.Stream (SP.Of (BioSequenceWindow w ty k)) m r
--{-# Inlinable attachSuffixes #-}
--attachSuffixes xs = undefined


-- * DNA/RNA

-- | Simple case translation from @U@ to @T@. with upper and lower-case
-- awareness.

rna2dna :: Char -> Char
rna2dna = \case
  'U' -> 'T'
  'u' -> 't'
  x   -> x
{-# Inline rna2dna #-}

-- | Single character RNA complement.

rnaComplement :: Char -> Char
rnaComplement = \case
  'A' -> 'U'
  'a' -> 'u'
  'C' -> 'G'
  'c' -> 'g'
  'G' -> 'C'
  'g' -> 'c'
  'U' -> 'A'
  'u' -> 'a'
  x   -> x
{-# Inline rnaComplement #-}

-- | Simple case translation from @T@ to @U@ with upper- and lower-case
-- awareness.

dna2rna :: Char -> Char
dna2rna = \case
  'T' -> 'U'
  't' -> 'u'
  x   -> x
{-# Inline dna2rna #-}

-- | Single character DNA complement.

dnaComplement :: Char -> Char
dnaComplement = \case
  'A' -> 'T'
  'a' -> 't'
  'C' -> 'G'
  'c' -> 'g'
  'G' -> 'C'
  'g' -> 'c'
  'T' -> 'A'
  't' -> 'a'
  x   -> x
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
  type TranscribeTo f :: *
  transcribe :: Iso' f (TranscribeTo f)

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
  complement :: Iso' f f

instance Complement (BioSequence DNA) where
  {-# Inline complement #-}
  complement = let f = (over _BioSequence (BS.map dnaComplement))
                   {-# Inline f #-}
               in  iso f f

instance Complement (BioSequence RNA) where
  {-# Inline complement #-}
  complement = let f = (over _BioSequence (BS.map rnaComplement))
                   {-# Inline f #-}
               in  iso f f

instance (Complement (BioSequence ty)) => Complement (BioSequenceWindow w ty k) where
  {-# Inline complement #-}
  complement = let f = over bswSequence (view complement)
                   {-# Inline f #-}
               in  iso f f

reverseComplement :: (Complement f, Reversing f) => Iso' f f
{-# Inline reverseComplement #-}
reverseComplement = reversed . complement


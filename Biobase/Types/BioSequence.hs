
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
import qualified Streaming.Internal as SI
import qualified Test.QuickCheck as TQ
import           Test.QuickCheck (Arbitrary(..))
import Data.Coerce
import Debug.Trace

import Biobase.Types.Strand
import qualified Biobase.Types.Index as BTI
import Data.Info



-- * Lens operations on biosequences

{-
class BioSeqLenses b where
  -- | Lens into the first @k@ characters.
  bsTake :: Int -> Lens' b b
  -- | Lens into the last @k@ characters
  bsTakeEnd :: Int -> Lens' b b
  -- | Lens into all but the first @k@ characters
  bsDrop :: Int -> Lens' b b
  -- | Lens into all but the last @k@ characters
  bsDropEnd :: Int -> Lens' b b
  -- | Lens that splits at a position
  bsSplitAt :: Int -> Lens' b (b,b)
  -- | length of this biosequence
  bsLength :: Getter b Int
-}

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



-- |
-- TODO provide extended annotation information on biosequences, too!

newtype BioSequence (which :: k) = BioSequence {_bioSequence :: ByteString}
  deriving stock (Data, Typeable, Generic, Eq, Ord, Read, Show)
  deriving newtype (Semigroup)
makeWrapped ''BioSequence
makePrisms ''BioSequence
makeLenses ''BioSequence

instance NFData (BioSequence w)

type instance Index (BioSequence w) = Int

type instance IxValue (BioSequence w) = Char

instance Ixed (BioSequence w) where
  ix k = _BioSequence . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving newtype instance Reversing (BioSequence w)

instance IsString (BioSequence Void) where
  fromString = BioSequence . BS.pack

instance Info (BioSequence w) where
  info (BioSequence s)
    | BS.length s <= 18 = BS.unpack s
    | otherwise         = BS.unpack h ++ ".." ++ BS.unpack l
    where (h,tl) = BS.splitAt 9 s
          (_,l ) = BS.splitAt (BS.length tl-9) tl

{-
instance BioSeqLenses (BioSequence w) where
  {-# Inline bsTake #-}
  bsTake k = lens (over _BioSequence (BS.take k)) (\old new -> new <> over _BioSequence (BS.drop k) old)
  {-# Inline bsTakeEnd #-}
  bsTakeEnd k = lens (over _BioSequence (\s -> BS.drop (BS.length s -k) s)) (\old new -> over _BioSequence (\s -> BS.take (BS.length s-k) s) old <> new)
  {-# Inline bsLength #-}
  bsLength = _BioSequence.to BS.length
  {-# Inline bsDrop #-}
  bsDrop k = lens (over _BioSequence (BS.drop k)) (\old new -> over _BioSequence (BS.take k) old <> new)
  {-# Inline bsDropEnd #-}
  bsDropEnd k = lens (over _BioSequence (\s -> BS.take (BS.length s -k) s)) (\old new -> over _BioSequence (\s -> BS.take (BS.length s-k) s) old <> new)
  {-# Inline bsSplitAt #-}
  bsSplitAt k = lens (\b -> (view (bsTake k) b, view (bsDrop k) b)) (\old (h,t) -> h <> t)
-}



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



{-

-- * A window into a longer sequence with prefix/suffix information.

-- | Phantom-typed over two types, the type @w@ of the identifier, which can be
-- descriptive ("FirstInput") and the second type, identifying what kind of
-- sequence types we are dealing with. Finally, the third type provides
-- location information and should be location or streamed location.

data BioSequenceWindow w ty loc = BioSequenceWindow
  { _bswIdentifier    :: !(SequenceIdentifier w)
    -- ^ Identifier for this window. Typically some fasta identifier
  , _bswPrefix        :: !(BioSequence ty)
  , _bswInfix         :: !(BioSequence ty)
  , _bswSuffix        :: !(BioSequence ty)
  , _bswInfixLocation :: !loc
    -- ^ Location of the infix sequence
  }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''BioSequenceWindow

-- | Lens into the full sequence. May not change the sequence length

bswSequence :: Lens (BioSequenceWindow w ty loc) (BioSequenceWindow w ty' loc) (BioSequence ty) (BioSequence ty')
{-# Inlinable bswSequence #-}
bswSequence = lens (\w -> _bswPrefix w <> _bswInfix w <> _bswSuffix w)
                   (\w bs -> let (p,is) = bs^.bsSplitAt (w^.bswPrefix.bsLength)
                                 (i,s ) = is^.bsSplitAt (w^.bswInfix.bsLength)
                             in w { _bswPrefix = p, _bswInfix = i, _bswSuffix = s } )

-- | Get the position of the whole sequence

bswLocation :: ModifyLocation loc => Getter (BioSequenceWindow w ty loc) loc
{-# Inlinable bswLocation #-}
bswLocation = to $ \w -> locMoveLeftEnd (w^.bswPrefix.bsLength.to negate)
                 . locMoveRightEnd (w^.bswSuffix.bsLength) $ w^.bswInfixLocation

bswRetagW :: BioSequenceWindow w ty loc -> BioSequenceWindow v ty loc
{-# Inlinable bswRetagW #-}
bswRetagW = over bswIdentifier coerce

instance NFData loc => NFData (BioSequenceWindow w ty loc)

instance (Reversing loc) => Reversing (BioSequenceWindow w ty loc) where
  {-# Inlinable reversing #-}
  reversing bsw = bsw
                & bswPrefix .~ (bsw^.bswSuffix.reversed)
                & bswSuffix .~ (bsw^.bswPrefix.reversed)
                & bswInfix  .~ (bsw^.bswInfix.reversed)
                & bswInfixLocation .~ (bsw^.bswInfixLocation.reversed)



-- | Provides an informative string indicating the current window being worked on. Requires length
-- of pretty string requested. Not for computers, but for logging what is being worked on. Should be
-- one line at most, not produce line breaks.
--
-- @...PFX [Start] IFX...IFX [End] SFX ...@
--
-- TODO possibly be better as a @Doc@ for prettier printing.

instance Info (BioSequenceWindow w ty loc) where
  info bsw = "todo: info bsw"

-- | For each element, attach the prefix as well. The @Int@ indicates the maximal prefix length to
-- attach.
--
-- @1 2 3 4@ -> @01 12 23 34@
--
-- TODO are we sure this is correct for @MinusStrand@?

attachPrefixes
  :: Monad m
  => Int
  -> SP.Stream (SP.Of (BioSequenceWindow w ty FwdLocation)) m r
  -> SP.Stream (SP.Of (BioSequenceWindow w ty FwdLocation)) m r
{-# Inlinable attachPrefixes #-}
attachPrefixes k = SP.map (\(Just w) -> w) . SP.drop 1 . SP.scan go Nothing id
  where
    go Nothing = Just
    go (Just p) = Just . set bswPrefix (view (bswInfix.bsTakeEnd k) p)

-- | For each element, attach the suffix as well.
--
-- @1 2 3 4@ -> @12 23 34 40@

attachSuffixes
  :: Monad m
  => Int
  -> SP.Stream (SP.Of (BioSequenceWindow w ty FwdLocation)) m r
  -> SP.Stream (SP.Of (BioSequenceWindow w ty FwdLocation)) m r
{-# Inlinable attachSuffixes #-}
attachSuffixes k = loop Nothing
  where
    loop Nothing = \case
      SI.Return r -> SI.Return r
      SI.Effect m -> SI.Effect $ fmap (loop Nothing) m
      SI.Step (a SP.:> rest) -> loop (Just a) rest
    loop (Just p) = \case
      SI.Return r -> SI.Step (p SP.:> SI.Return r)
      SI.Effect m -> SI.Effect $ fmap (loop (Just p)) m
      SI.Step (a SP.:> rest) ->
        let p' = p & set bswSuffix (view (bswInfix.bsTake k) a)
        in  SI.Step (p' SP.:> loop (Just a) rest)

-}



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

{-
instance (Complement (BioSequence ty)) => Complement (BioSequenceWindow w ty k) where
  {-# Inline complement #-}
  complement = let f = over bswPrefix (view complement) . over bswInfix (view complement) . over bswSuffix (view complement)
                   {-# Inline f #-}
               in  iso f f
-}

reverseComplement :: (Complement f, Reversing f) => Iso' f f
{-# Inline reverseComplement #-}
reverseComplement = reversed . complement


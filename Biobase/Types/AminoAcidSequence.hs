
-- | Encode the allowed amino acids in a better way.

module Biobase.Types.AminoAcidSequence where

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



-- | A short amino acid suquence.
--
-- It is an instance of 'Ixed' to allow @RNAseq (BS.pack "cag") ^? ix 2 == Just 'g'@.

newtype AAseq = AAseq { _aaseq ∷ ByteString }
  deriving (Data, Typeable, Generic, Eq, Ord, Read, Show)
makeLenses ''AAseq

instance NFData AAseq

type instance Index AAseq = Int

type instance IxValue AAseq = Char

instance Ixed AAseq where
  ix k = aaseq . ix k . iso (chr . fromIntegral) (fromIntegral . ord)
  {-# Inline ix #-}

deriving instance Reversing AAseq

mkAAseq ∷ ByteString → AAseq
mkAAseq = AAseq . BS.map go . BS.map toUpper
  where go x | x `elem` aas = x
             | otherwise    = 'X'
        aas ∷ String
        aas = "ARNDCEQGHILKMFPSTWYVUO"

instance IsString AAseq where
  fromString = mkAAseq . BS.pack

instance Arbitrary AAseq where
  arbitrary = do
    k ← TQ.choose (0,100)
    xs ← TQ.vectorOf k $ TQ.elements "ARNDCEQGHILKMFPSTWYVUO"
    return . AAseq $ BS.pack xs
  shrink = view (to shrink)




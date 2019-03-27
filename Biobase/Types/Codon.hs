
module Biobase.Types.Codon where

import Control.Lens
import GHC.Generics (Generic)



-- | A single codon.
--
-- TODO needs to go into its own place

data Codon c = Codon !c !c !c
  deriving (Eq,Ord,Read,Show,Generic,Functor,Foldable,Traversable)

instance Field1 (Codon c) (Codon c) c c
instance Field2 (Codon c) (Codon c) c c
instance Field3 (Codon c) (Codon c) c c
instance Each (Codon c) (Codon c') c c'


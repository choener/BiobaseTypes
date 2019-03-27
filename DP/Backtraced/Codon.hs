
-- | The Backtraced column structure is for codon-based alignments, including
-- special cases.

module DP.Backtraced.Codon where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import Biobase.Types.Codon



-- | A single 'Backtraced' column. Since such a column will be part of a
-- @Backtraced (Z:.BtCodon c aa:. ...)@ structure, it is always possible to
-- extend even further, by having more entries.

data BtCodon c aa
  -- | A canonical match. A codon and the translated amino acid need to be set.
  = Match
    { _codon  ∷ !(Codon c)
    , _aa     ∷ !aa
    }
  -- | A frameshifting match. The vector of frameshifted nucleotides will have
  -- a number of characters @c@, that encode for a single amino acid.
  | Frameshift
    { _frameshift ∷ !(Vector c)
    , _aa         ∷ !aa
    }
  | Insert
    { _codon  ∷ !(Codon c)
    , _aa     ∷ !aa
    }
  | Shifted
    { _frameshift ∷ !(Vector c)
    , _aa         ∷ !aa
    }
  | Region
    { _region     ∷ !(Vector c)
    , _annotation ∷ !ByteString
    }
  | Delete
    {
    }


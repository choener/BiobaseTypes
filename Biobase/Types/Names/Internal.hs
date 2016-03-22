
module Biobase.Types.Names.Internal where

import Data.IORef (newIORef,IORef,readIORef,atomicWriteIORef,atomicModifyIORef')
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)

import Data.Bijection.HashMap
import Data.Bijection.Vector



speciesNameBimap :: IORef (Bimap (HashMap Text Int) (Vector Text))
speciesNameBimap = unsafePerformIO $ newIORef empty
{-# NoInline speciesNameBimap #-}

-- | Add @Text@ and return @Int@ key. Will return key for
-- existing string and thereby serves for lookup in left-to-right
-- direction.

speciesNameBimapAdd :: Text -> Int
speciesNameBimapAdd k = unsafeDupablePerformIO $ atomicModifyIORef' speciesNameBimap $ \m ->
  case lookupL m k of Just i  -> (m,i)
                      Nothing -> let s = size m
                                 in  (insert m (k,s) , s)
{-# Inline speciesNameBimapAdd #-}

-- | Lookup the @InternedMultiChar@ based on an @Int@ key. Unsafe totality
-- assumption.

speciesNameBimapLookupInt :: Int -> Text
speciesNameBimapLookupInt r = seq r . unsafeDupablePerformIO $ atomicModifyIORef' speciesNameBimap $ \m ->
  case lookupR m r of Just l  -> (m,l)
                      Nothing -> error "speciesNameBimapLookupInt: totality assumption invalidated"
{-# Inline speciesNameBimapLookupInt #-}


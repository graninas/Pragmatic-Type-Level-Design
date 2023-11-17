{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Common.NatList where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)


class ToIntList (ns :: [Nat]) where
  toIntList :: Proxy ns -> [Int]

instance ToIntList '[] where
  toIntList _ = []

instance (KnownNat  c, ToIntList cs) =>
  ToIntList (c ': cs) where
  toIntList _
    = fromIntegral (natVal (Proxy @c))
    : toIntList (Proxy @cs)

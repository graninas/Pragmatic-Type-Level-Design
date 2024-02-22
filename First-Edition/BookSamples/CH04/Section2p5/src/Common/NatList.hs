{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Common.NatList where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Common.NonEmptyList


data SupportedList where
  NatList :: [Nat] -> SupportedList
  CList1  :: CustomList1 Nat -> SupportedList
  CList2  :: CustomList2 Nat -> SupportedList

class ToIntList (l :: SupportedList) where
  toIntList :: Proxy l -> [Int]

instance ToIntList ('NatList '[]) where
  toIntList _ = []

instance
  ( KnownNat c
  , ToIntList ('NatList cs)
  ) =>
  ToIntList ('NatList (c ': cs)) where
  toIntList _
    = toInt' (Proxy @c)
    : toIntList (Proxy @('NatList cs))


-- List1
instance
  ( KnownNat c1
  , ToIntList ('NatList cs)
  ) =>
  ToIntList ('CList1 ('List1 c1 cs)) where
  toIntList _
    = toInt' (Proxy @c1)
    : toIntList (Proxy @('NatList cs))


-- List2
instance
  ( KnownNat c1
  , KnownNat c2
  , ToIntList ('NatList cs)
  ) =>
  ToIntList ('CList2 (List2 c1 c2 cs)) where
  toIntList _
    = toInt' (Proxy @c1)
    : toInt' (Proxy @c2)
    : toIntList (Proxy @('NatList cs))





toInt' :: KnownNat p => Proxy p -> Int
toInt' p = fromIntegral (natVal p)

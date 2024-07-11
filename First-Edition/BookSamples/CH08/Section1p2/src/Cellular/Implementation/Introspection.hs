{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}

module Cellular.Implementation.Introspection where

import CPrelude

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )

import Cellular.Language.Automaton
import Cellular.Extensions.Automaton


class Introspect (it :: a) where
  introspect :: Proxy it -> [String]

data Counts a

instance
  ( Introspect st
  ) =>
  Introspect ('StateWrapper st) where
  introspect _ = introspect $ Proxy @st

instance
  ( KnownSymbol name
  , KnownNat idx
  ) =>
  Introspect (StateImpl name idx) where
  introspect _ =
    [ "StateImpl"
    , symbolVal $ Proxy @name
    , show $ natVal $ Proxy @idx
    ]

instance Introspect (Counts '[]) where
  introspect _ = []

instance
  ( KnownNat cnt
  , Introspect (Counts cnts)
  ) =>
  Introspect (Counts (cnt ': cnts)) where
  introspect _ =
    (show $ natVal $ Proxy @cnt)
    : introspect (Proxy @(Counts cnts))


instance
  ( Introspect st
  , Introspect (Counts cnts)
  ) =>
  Introspect (NeighborsCountImpl st cnts) where
  introspect _ =
    [ "NeighborsCountImpl"
    ] <> introspect (Proxy @st)
      <> introspect (Proxy @(Counts cnts))

instance
  ( Introspect cond
  ) =>
  Introspect ('CellConditionWrapper cond) where
  introspect _ = introspect $ Proxy @cond

instance
  ( Introspect fromSt
  , Introspect toSt
  , Introspect cond
  ) =>
  Introspect ('StateTransition fromSt toSt cond) where
  introspect _ = let
    strs1 = introspect $ Proxy @fromSt
    strs2 = introspect $ Proxy @toSt
    strs3 = introspect $ Proxy @cond
    in "StateTransition" : strs1 <> strs2 <> strs3

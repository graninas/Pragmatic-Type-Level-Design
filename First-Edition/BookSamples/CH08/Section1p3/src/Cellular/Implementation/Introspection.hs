{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cellular.Implementation.Introspection where

import CPrelude

import TypeLevelDSL.Eval
import GHC.TypeLits
import Data.Proxy ( Proxy(..) )

import Cellular.Language.Automaton
import Cellular.Extensions.Automaton


data Introspect = Introspect

data Counts a

instance
  ( Eval () Introspect st [String]
  ) =>
  Eval () Introspect ('StateWrapper st) [String] where
  eval _ _ _ = eval () Introspect $ Proxy @st

instance
  ( KnownSymbol name
  , KnownNat idx
  ) =>
  Eval () Introspect (StateImpl name idx) [String] where
  eval _ _ _ =
    [ "StateImpl"
    , symbolVal $ Proxy @name
    , show $ natVal $ Proxy @idx
    ]

instance
  Eval () Introspect (Counts '[]) [String] where
  eval _ _ _ = []

instance
  ( KnownNat cnt
  , Eval () Introspect (Counts cnts) [String]
  ) =>
  Eval () Introspect (Counts (cnt ': cnts)) [String] where
  eval _ _ _ =
    (show $ natVal $ Proxy @cnt)
    : eval () Introspect (Proxy @(Counts cnts))


instance
  ( Eval () Introspect st [String]
  , Eval () Introspect (Counts cnts) [String]
  ) =>
  Eval () Introspect (NeighborsCountImpl st cnts) [String] where
  eval _ _ _ =
    [ "NeighborsCountImpl"
    ] <> eval () Introspect (Proxy @st)
      <> eval () Introspect (Proxy @(Counts cnts))

instance
  ( Eval () Introspect cond [String]
  ) =>
  Eval () Introspect ('CellConditionWrapper cond) [String] where
  eval _ _ _ = eval () Introspect $ Proxy @cond

instance
  ( Eval () Introspect fromSt [String]
  , Eval () Introspect toSt [String]
  , Eval () Introspect cond [String]
  ) =>
  Eval () Introspect ('StateTransition fromSt toSt cond) [String] where
  eval _ _ _ = let
    strs1 = eval () Introspect $ Proxy @fromSt
    strs2 = eval () Introspect $ Proxy @toSt
    strs3 = eval () Introspect $ Proxy @cond
    in "StateTransition" : strs1 <> strs2 <> strs3

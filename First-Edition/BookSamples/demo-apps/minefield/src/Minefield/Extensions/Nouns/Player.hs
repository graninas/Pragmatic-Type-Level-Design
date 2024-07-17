
module Minefield.Extensions.Nouns.Player where

import CPrelude

import TypeLevelDSL.Eval
import Minefield.Core.Types
import Minefield.Core.Interface
import Minefield.Core.Object

import Minefield.Extensions.Materialization

import GHC.TypeLits


data PlayerImpl
  (icon :: Symbol)
  (objectType :: Symbol)
type Player i = MkObject (PlayerImpl i "player")

-- Implementation

instance
  ( KnownSymbol i
  ) =>
  EvalIO () GetIcon (PlayerImpl i ot) Icon where
  evalIO () _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  EvalIO () GetObjectInfo (PlayerImpl i ot) ObjectInfo where
  evalIO () _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon = head $ symbolVal $ Proxy @i
    pure $ ObjectInfo icon (-1, -1) oType True []

instance
  ( KnownSymbol ot
  ) =>
  EvalIO () GetObjectType (PlayerImpl i ot) ObjectType where
  evalIO () _ _ = pure $ symbolVal $ Proxy @ot

-- Object

data PlayerObject = PlayerObject
  { poObjectInfoRef :: IORef ObjectInfo
  }

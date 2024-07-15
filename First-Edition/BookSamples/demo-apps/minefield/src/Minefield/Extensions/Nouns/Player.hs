
module Minefield.Extensions.Nouns.Player where

import CPrelude

import Minefield.Core.Eval
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
  Eval () GetIcon (PlayerImpl i ot) Icon where
  eval () _ _ = pure $ head $ symbolVal $ Proxy @i

instance
  ( KnownSymbol i
  , KnownSymbol ot
  ) =>
  Eval () GetObjectInfo (PlayerImpl i ot) (ObjectType, Icon) where
  eval () _ _ = do
    let oType = symbolVal $ Proxy @ot
    let icon = head $ symbolVal $ Proxy @i
    pure (oType, icon)

instance
  ( KnownSymbol ot
  ) =>
  Eval () GetObjectType (PlayerImpl i ot) ObjectType where
  eval () _ _ = pure $ symbolVal $ Proxy @ot

-- Object

data PlayerObject = PlayerObject
  { pObjectInfo :: ObjectInfo
  }

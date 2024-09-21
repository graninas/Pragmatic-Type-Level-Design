{-# LANGUAGE UndecidableInstances #-}

module Minefield.Implementation.SystemNouns.Player.Template where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Implementation.SystemNouns.Player.Def

import GHC.TypeLits


-- TODO: replace with a common template def

-- Implementation of the template

instance
  ( KnownSymbol i
  ) =>
  EvalIO () GetIcon (PlayerDef i ot) Icon where
  evalIO () _ _ = pure $ head $ symbolVal $ Proxy @i

-- -- Get object type of the template
instance
  ( KnownSymbol ot
  ) =>
  EvalIO () GetObjectType (PlayerDef i ot) ObjectType where
  evalIO () _ _ = pure $ symbolVal $ Proxy @ot

-- -- Get object info of the template
instance
  ( t ~ PlayerDef i ot
  , EvalIO () GetIcon t Icon
  , EvalIO () GetObjectType t ObjectType
  ) =>
  EvalIO () GetObjectInfo (PlayerDef i ot) ObjectInfo where
  evalIO () _ _ = do
    oType <- evalIO () GetObjectType $ Proxy @t
    icon  <- evalIO () GetIcon $ Proxy @t
    pure $ ObjectInfo oType noObjectId True (icon, [])


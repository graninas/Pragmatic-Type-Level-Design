{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Nouns.TimerBomb.Template where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Core.Defaults
import Minefield.Extensions.Nouns.TimerBomb.Def

import GHC.TypeLits


-- TODO: replace with a common template def

-- Implementation of the template

-- -- Get icon of the template
instance
  ( KnownSymbol i
  ) =>
  EvalIO () GetIcon (TimerBombDef i ot turns) Icon where
  evalIO () _ _ = pure $ head $ symbolVal $ Proxy @i

-- -- Get object type of the template
instance
  ( KnownSymbol ot
  ) =>
  EvalIO () GetObjectType (TimerBombDef i ot t) ObjectType where
  evalIO () _ _ = pure $ symbolVal $ Proxy @ot

-- -- Get object info of the template
instance
  ( t ~ TimerBombDef i ot turns
  , EvalIO () GetIcon t Icon
  , EvalIO () GetObjectType t ObjectType
  ) =>
  EvalIO () GetObjectInfo (TimerBombDef i ot turns) ObjectInfo where
  evalIO () _ _ = do
    oType <- evalIO () GetObjectType $ Proxy @t
    icon  <- evalIO () GetIcon $ Proxy @t
    pure $ ObjectInfo oType noObjectId True (icon, [])



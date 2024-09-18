{-# LANGUAGE UndecidableInstances #-}

module Minefield.Extensions.Nouns.Landmine.Template where

import CPrelude

import TypeLevelDSL.Eval

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Extensions.Nouns.Landmine.Def

import GHC.TypeLits


-- TODO: replace with a common template def

-- Implementation of the template

-- -- Get icon of the template
getLandmineIcon :: Int -> Icon
getLandmineIcon p = ['A'..'Z'] !! (p - 1)

instance
  ( KnownSymbol i
  ) =>
  EvalIO () GetIcon (LandmineDef i ot p) Icon where
  evalIO () _ _ = pure $ head $ symbolVal $ Proxy @i
  -- evalIO () _ _ = pure
  --   $ getLandmineIcon
  --   $ fromIntegral
  --   $ natVal
  --   $ Proxy @p

-- -- Get object type of the template
instance
  ( KnownSymbol ot
  ) =>
  EvalIO () GetObjectType (LandmineDef i ot p) ObjectType where
  evalIO () _ _ = pure $ symbolVal $ Proxy @ot

-- -- Get object info of the template
instance
  ( t ~ LandmineDef i ot p
  , EvalIO () GetIcon t Icon
  , EvalIO () GetObjectType t ObjectType
  ) =>
  EvalIO () GetObjectInfo (LandmineDef i ot p) ObjectInfo where
  evalIO () _ _ = do
    oType <- evalIO () GetObjectType $ Proxy @t
    icon  <- evalIO () GetIcon $ Proxy @t
    pure $ ObjectInfo oType Nothing True Nothing (icon, [])

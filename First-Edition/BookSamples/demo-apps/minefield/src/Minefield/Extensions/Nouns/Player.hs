
module Minefield.Extensions.Nouns.Player where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.System
import Minefield.Core.Eval

import GHC.TypeLits


data PlayerImpl
  (icon :: Symbol)
type Player i = MkObject (PlayerImpl i)



instance
  ( KnownSymbol i
  ) =>
  Eval GetIcon (PlayerImpl i) Char where
  eval _ _ = head $ symbolVal $ Proxy @i

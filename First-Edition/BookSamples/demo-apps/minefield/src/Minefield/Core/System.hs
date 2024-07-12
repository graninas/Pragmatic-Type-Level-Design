module Minefield.Core.System where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Language

import GHC.TypeLits


data GetIcon


instance
  ( Eval GetIcon o Char
  ) =>
  Eval GetIcon ('ObjectWrapper o) Char where
  eval vProxy _ = eval vProxy $ Proxy @o


data Objects a

instance
  Eval GetIcon (Objects '[]) [Char] where
  eval _ _ = []

instance
  ( Eval GetIcon o Char
  , Eval GetIcon (Objects os) [Char]
  ) =>
  Eval GetIcon (Objects (o ': os)) [Char] where
  eval vProxy _ = let
    o  = eval vProxy $ Proxy @o
    os = eval vProxy $ Proxy @(Objects os)
    in o : os

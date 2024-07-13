module Minefield.Extensions.Materialization where

import CPrelude

import Minefield.Core.Eval
import Minefield.Core.Language

import Minefield.Game.Types

import GHC.TypeLits


data GetIcon
data MakeCommands
data MakeCommand

data Objects a
data ObjsActs objs acts

instance
  ( Eval GetIcon o Char
  ) =>
  Eval GetIcon ('ObjectWrapper o) Char where
  eval vProxy _ = eval vProxy $ Proxy @o

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

instance
  Eval MakeCommands (ObjsActs objects actions) PlayerCommands where
  eval _ _ = 0

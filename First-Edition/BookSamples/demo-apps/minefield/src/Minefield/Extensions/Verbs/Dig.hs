module Minefield.Extensions.Verbs.Dig where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


data DigDef

type Dig = MkAction DigDef "dig" 'True

module Minefield.Extensions.Verbs.Dig where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits

data DigImpl

type Dig = MkAction DigImpl 'True "dig"

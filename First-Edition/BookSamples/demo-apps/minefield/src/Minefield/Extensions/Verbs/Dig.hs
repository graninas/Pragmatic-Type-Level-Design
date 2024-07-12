module Minefield.Extensions.Verbs.Dig where

import CPrelude

import Minefield.Core.Language

import GHC.TypeLits

data DigImpl

type Dig = MkAction DigImpl

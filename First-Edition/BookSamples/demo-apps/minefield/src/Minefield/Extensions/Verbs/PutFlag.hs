module Minefield.Extensions.Verbs.PutFlag where

import CPrelude

import Minefield.Core.Language

import GHC.TypeLits


data PutFlagImpl

type PutFlag = MkAction PutFlagImpl


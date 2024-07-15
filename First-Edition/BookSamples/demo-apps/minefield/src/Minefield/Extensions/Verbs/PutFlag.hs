module Minefield.Extensions.Verbs.PutFlag where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


data PutFlagImpl

type PutFlag = MkAction PutFlagImpl 'True "flag"


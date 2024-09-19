module Minefield.Implementation.SystemVerbs.Walk where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


data WalkDef

type Walk = MkAction WalkDef "" 'True


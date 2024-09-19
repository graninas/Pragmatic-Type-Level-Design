module Minefield.Implementation.SystemVerbs.PutFlag where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


data PutFlagDef

type PutFlag = MkAction PutFlagDef "flag" 'True


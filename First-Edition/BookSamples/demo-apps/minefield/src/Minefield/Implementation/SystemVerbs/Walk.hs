module Minefield.Implementation.SystemVerbs.Walk where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


data WalkImpl

type Walk = MkAction WalkImpl "" 'True


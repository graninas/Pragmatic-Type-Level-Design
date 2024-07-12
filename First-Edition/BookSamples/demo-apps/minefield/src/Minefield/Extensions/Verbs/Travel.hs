module Minefield.Extensions.Verbs.Travel where

import CPrelude

import Minefield.Core.Language

import GHC.TypeLits


data TravelImpl
data StayImpl

type Travel = MkAction TravelImpl
type Stay = MkAction StayImpl

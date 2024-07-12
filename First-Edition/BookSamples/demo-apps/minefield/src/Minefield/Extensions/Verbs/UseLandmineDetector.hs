module Minefield.Extensions.Verbs.UseLandmineDetector where

import CPrelude

import Minefield.Core.Language

import GHC.TypeLits

data UseLandmineDetectorImpl

type UseLandmineDetector = MkAction UseLandmineDetectorImpl

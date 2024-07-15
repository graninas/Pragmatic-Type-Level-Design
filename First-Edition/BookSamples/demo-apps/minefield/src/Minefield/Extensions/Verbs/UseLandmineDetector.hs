module Minefield.Extensions.Verbs.UseLandmineDetector where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits

data UseLandmineDetectorImpl

type UseLandmineDetector =
  MkAction UseLandmineDetectorImpl 'True "scan"

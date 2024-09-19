module Minefield.Extensions.Verbs.UseLandmineDetector where

import CPrelude

import Minefield.Core.Interface

import GHC.TypeLits


data UseLandmineDetectorDef

type UseLandmineDetector =
  MkAction UseLandmineDetectorDef "scan" 'True

{-# LANGUAGE DataKinds #-}
module Cellular.Assets.Automata.LifeLike where

import Cellular.Language.Algorithm
import Common.NonEmptyList


type A = 1
type D = 0

type Alive = 'State "Alive" A
type Dead  = 'State "Dead"  D

type LifeLikeStates = 'List2
  Alive
  Dead
  '[]

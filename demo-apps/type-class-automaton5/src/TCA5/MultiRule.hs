module TCA5.MultiRule where

import CPrelude


import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton

import qualified TCA5.GameOfLife as GoL
import qualified TCA5.Arbitrary2S as A2S
import qualified TCA5.Arbitrary3S as A3S

data MultiRule
  = GameOfLifeRule
  | Arbitrary2SRule
  | Arbitrary3SRule
  deriving (Show, Eq, Ord, Enum)

data MultiCell = MC0 | MC1 | MC2
  deriving (Show, Eq, Ord)


instance Dim2Automaton MultiRule MultiCell where
  emptyCell _ = MC0
  evolve rule = rule
  step = adaptedStep


class MultiCellConvert foreignCell where
  toMultiCell  :: foreignCell -> MultiCell
  fromMultiCell :: MultiCell -> foreignCell

instance MultiCellConvert GoL.GoLCell where
  toMultiCell GoL.GoLDead  = MC0
  toMultiCell GoL.GoLAlive = MC1
  fromMultiCell MC0 = GoL.GoLDead
  fromMultiCell MC1 = GoL.GoLAlive

instance MultiCellConvert A2S.Arbitrary2SCell where
  toMultiCell A2S.A2SDead  = MC0
  toMultiCell A2S.A2SAlive = MC1
  fromMultiCell MC0 = A2S.A2SDead
  fromMultiCell MC1 = A2S.A2SAlive

instance MultiCellConvert A3S.Arbitrary3SCell where
  toMultiCell A3S.A3S0 = MC0
  toMultiCell A3S.A3S1 = MC1
  toMultiCell A3S.A3S2 = MC2
  fromMultiCell MC0 = A3S.A3S0
  fromMultiCell MC1 = A3S.A3S1
  fromMultiCell MC2 = A3S.A3S2


adaptedStep :: MultiRule -> Dim2Board MultiCell -> Dim2Board MultiCell
adaptedStep GameOfLifeRule board
  = fmap toMultiCell
  $ GoL.golStep
  $ fmap fromMultiCell board

adaptedStep Arbitrary2SRule board
  = fmap toMultiCell
  $ A2S.step'
  $ fmap fromMultiCell board

adaptedStep Arbitrary3SRule board
  = fmap toMultiCell
  $ A3S.step'
  $ fmap fromMultiCell board

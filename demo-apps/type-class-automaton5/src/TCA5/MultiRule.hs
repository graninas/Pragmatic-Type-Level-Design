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
  = GameOfLifeMultiRule
  | Arbitrary2SMultiRule
  | Arbitrary3SMultiRule
  deriving (Show, Eq, Ord, Enum)

data MultiCell = MC0 | MC1 | MC2
  deriving (Show, Eq, Ord)


instance Dim2Automaton MultiRule MultiCell where
  emptyCell = adaptedEmptyCell
  evolve = adaptedEvolve
  step = adaptedStep


class MultiCellConvert foreignCell where
  toMultiCell  :: foreignCell -> MultiCell
  fromMultiCell :: MultiCell -> foreignCell

instance MultiCellConvert GoL.GoLCell where
  toMultiCell GoL.GoLDead  = MC0
  toMultiCell GoL.GoLAlive = MC1
  fromMultiCell MC0 = GoL.GoLDead
  fromMultiCell MC1 = GoL.GoLAlive

instance MultiCellConvert A2S.A2SCell where
  toMultiCell A2S.A2SDead  = MC0
  toMultiCell A2S.A2SAlive = MC1
  fromMultiCell MC0 = A2S.A2SDead
  fromMultiCell MC1 = A2S.A2SAlive

instance MultiCellConvert A3S.A3SCell where
  toMultiCell A3S.A3SC0 = MC0
  toMultiCell A3S.A3SC1 = MC1
  toMultiCell A3S.A3SC2 = MC2
  fromMultiCell MC0 = A3S.A3SC0
  fromMultiCell MC1 = A3S.A3SC1
  fromMultiCell MC2 = A3S.A3SC2


adaptedEmptyCell :: MultiRule -> MultiCell
adaptedEmptyCell rule = case rule of
  GameOfLifeMultiRule  -> toMultiCell $ emptyCell GoL.GoL
  Arbitrary2SMultiRule -> toMultiCell $ emptyCell A2S.A2S
  Arbitrary3SMultiRule -> toMultiCell $ emptyCell A3S.A3S




adaptedEvolve :: MultiRule -> MultiRule
adaptedEvolve = id

adaptedStep :: MultiRule -> Dim2Board MultiCell -> Dim2Board MultiCell
adaptedStep GameOfLifeMultiRule board
  = fmap toMultiCell
  $ GoL.golStep
  $ fmap fromMultiCell board

adaptedStep Arbitrary2SMultiRule board
  = fmap toMultiCell
  $ A2S.step'
  $ fmap fromMultiCell board

adaptedStep Arbitrary3SMultiRule board
  = fmap toMultiCell
  $ A3S.step'
  $ fmap fromMultiCell board

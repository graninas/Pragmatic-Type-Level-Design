module Main where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton
import TCA5.GameOfLife
import TCA5.Arbitrary2S
import TCA5.Arbitrary3S
import TCA5.MultiRule




main :: IO ()
main = do

  let golBoard1 = initializeBoard GameOfLife (50, 50) Map.empty
  let golBoard2 = step GameOfLife golBoard1

  let arbitrary3SBoard1 = initializeBoard Arbitrary3S (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step Arbitrary3S arbitrary3SBoard1

  let arbitrary2SBoard1 = initializeBoard Arbitrary2S (50, 50) Map.empty
  let arbitrary2SBoard2 = step Arbitrary2S arbitrary2SBoard1      -- was a copy-paste bug: arbitrary3SBoard1; => type unsafe

  -- Multi rule
  let multiRuleBoard1 = initializeBoard GameOfLifeRule (50, 50) Map.empty
  let multiRuleBoard2 = step GameOfLifeRule multiRuleBoard1      -- was a copy-paste bug: arbitrary3SBoard1; => type unsafe

  print golBoard2
  print arbitrary3SBoard2
  print arbitrary2SBoard2

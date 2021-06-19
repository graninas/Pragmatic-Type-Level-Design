module Main where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA2.Types
import TCA2.Automaton
import TCA2.GameOfLife
import TCA2.Arbitrary2S
import TCA2.Arbitrary3S




main :: IO ()
main = do

  let golBoard1 = initializeBoard @GoLRule @TwoStateCell (50, 50) Map.empty
  let golBoard2 = step golBoard1

  let arbitrary3SBoard1 = initializeBoard @Arbitrary3S @Arbitrary3SCell (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step arbitrary3SBoard1

  let arbitrary2SBoard1 = initializeBoard @Arbitrary2S @TwoStateCell (50, 50) Map.empty
  let arbitrary2SBoard2 = step arbitrary2SBoard1      -- was a copy-paste bug: arbitrary3SBoard1; => type unsafe

  print golBoard2
  print arbitrary3SBoard2
  print arbitrary2SBoard2

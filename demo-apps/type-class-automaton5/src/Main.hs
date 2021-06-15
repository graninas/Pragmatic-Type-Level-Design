module Main where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA5.Types
import TCA5.Automaton
import TCA5.GameOfLife
import TCA5.Arbitrary2S
import TCA5.Arbitrary3S




main :: IO ()
main = do

  let golBoard1 = initialize @GameOfLife @GoLCell (50, 50) Map.empty
  let golBoard2 = step golBoard1

  let arbitrary3SBoard1 = initialize @Arbitrary3S @Arbitrary3SCell (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step arbitrary3SBoard1

  let arbitrary2SBoard1 = initialize @Arbitrary2S @Arbitrary2SCell (50, 50) Map.empty
  let arbitrary2SBoard2 = step arbitrary2SBoard1      -- was a copy-paste bug: arbitrary3SBoard1; => type unsafe

  print golBoard2
  print arbitrary3SBoard2
  print arbitrary2SBoard2

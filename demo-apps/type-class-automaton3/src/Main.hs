module Main where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA3.Types
import TCA3.Automaton
import TCA3.GameOfLife
import TCA3.Arbitrary2S
import TCA3.Arbitrary3S




main :: IO ()
main = do

  let golBoard1 = initialize @GoLRule (50, 50) Map.empty
  let golBoard2 = step golBoard1

  let arbitrary3SBoard1 = initialize @Arbitrary3S (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step arbitrary3SBoard1

  let arbitrary2SBoard1 = initialize @Arbitrary2S (50, 50) Map.empty
  let arbitrary2SBoard2 = step arbitrary3SBoard1

  print golBoard2
  print arbitrary3SBoard2

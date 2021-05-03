module Main where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA.Types
import TCA.Automaton
import TCA.GameOfLife
import TCA.Arbitrary3S




main :: IO ()
main = do

  let golBoard1 = initialize @GoLCell (50, 50) Map.empty
  let golBoard2 = step golBoard1

  let arbitrary3SBoard1 = initialize (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step arbitrary3SBoard1

  print golBoard2
  print arbitrary3SBoard2

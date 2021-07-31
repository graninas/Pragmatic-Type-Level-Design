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

  let golBoard1 = initializeBoard GoL (50, 50) Map.empty
  let golBoard2 = step GoL golBoard1

  let arbitrary3SBoard1 = initializeBoard A3S (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step A3S arbitrary3SBoard1

  let arbitrary2SBoard1 = initializeBoard A2S (50, 50) Map.empty
  let arbitrary2SBoard2 = step A2S arbitrary2SBoard1

  -- Multi rule
  -- Step the board as GoL and then step it as A3S:
  let multiRuleBoard1 = initializeBoard GameOfLifeMultiRule (50, 50) Map.empty
  let multiRuleBoard2 = step GameOfLifeMultiRule multiRuleBoard1
  let multiRuleBoard3 = step Arbitrary2SMultiRule multiRuleBoard2
  let multiRuleBoard4 = step Arbitrary3SMultiRule multiRuleBoard3

  print golBoard2
  print arbitrary3SBoard2
  print arbitrary2SBoard2

  print multiRuleBoard1
  print multiRuleBoard2
  print multiRuleBoard3
  print multiRuleBoard4

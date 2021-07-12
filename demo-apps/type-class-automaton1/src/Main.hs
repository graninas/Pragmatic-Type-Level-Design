module Main where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Vector as V

import TCA1.Types
import TCA1.Automaton
import TCA1.GameOfLife
import TCA1.Arbitrary3S

import qualified TCA1.GameOfLife as GoL
import qualified TCA1.Arbitrary3S as A3S

mkBoard :: (Int, Int) -> cell -> Dim2Board cell
mkBoard (xSize, ySize) cell =
  Dim2Board cells xSize ySize
  where
    cells       = V.generate xSize generateX
    generateX _ = V.generate ySize generateY
    generateY _ = cell

main :: IO ()
main = do

  let testBoard1 = mkBoard (50, 50) Dead
  let testBoard2 = mkBoard (50, 50) $ Arbitrary3SCell 0

  let testBoard3 = GoL.golStep testBoard1


  let golBoard1 = initializeBoard @GoLCell (50, 50) Map.empty
  let golBoard2 = step golBoard1

  let arbitrary3SBoard1 = initializeBoard (50, 50) $ Map.fromList
          [((x, y), toArbitrary3SCell (x+y)) | x <- [0..10], y <- [0..10]]
  let arbitrary3SBoard2 = step arbitrary3SBoard1

  print golBoard2
  print arbitrary3SBoard2

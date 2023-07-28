module Main where

import Board (Board)
import Cell (Cell(..))
import GameOfLife (loadGoLFromFile, saveGoLToFile, iterateGoL)
import Seeds ( loadSeedsFromFile )

import qualified Data.Map as Map

glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

main :: IO ()
main = do
  gol1   <- loadGoLFromFile "./data/GoL/glider.txt"
  seeds1 <- loadSeedsFromFile "./data/Seeds/world1.txt"

  let gol2 = iterateGoL 5 gol1

  saveGoLToFile "./data/GoL/glider_5th_gen.txt" gol2

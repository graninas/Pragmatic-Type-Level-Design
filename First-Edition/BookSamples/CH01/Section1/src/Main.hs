module Main where

import Board (Board)
import Cell (Cell(..))
import GameOfLife (loadGoLFromFile, saveGoLToFile, iterateGoL)

import qualified Data.Map as Map

glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

main :: IO ()
main = do
  board1 <- loadGoLFromFile "./data/world1.txt"
  let board2 = iterateGoL 5 board1
  saveGoLToFile "./data/world1_5th_gen.txt" board2

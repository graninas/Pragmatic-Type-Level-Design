module Main where

import Board ( Board, saveToFile, loadFromFile )
import Cell (Cell(..))
import GameOfLife ( iterateWorld )

import qualified Data.Map as Map

glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

main :: IO ()
main = do
  board1 <- loadFromFile "./data/glider.txt"
  let board2 = iterateWorld 5 board1
  saveToFile "./data/glider_5th_gen.txt" board2

module Main where

import Board ( Board, saveToFile )
import Cell (Cell(..))
import GameOfLife
import Seeds

import qualified Data.Map as Map

glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

iterateWorld :: (ca -> ca) -> Int -> ca -> ca
iterateWorld step n world | n == 0 = world
iterateWorld step n world | n > 0 =
  head (drop n (iterate step world))
iterateWorld _ _ _ = error "Invalid iteration count"

saveToFile'' :: (ca -> Board) -> FilePath -> ca -> IO ()
saveToFile'' unwrap file world = saveToFile file (unwrap world)

unwrapSeeds :: Seeds -> Board
unwrapSeeds (Seeds world) = world

main :: IO ()
main = do
  gol1   <- loadGoLFromFile   "./data/GoL/glider.txt"
  seeds1 <- loadSeedsFromFile "./data/Seeds/world1.txt"

  let gol2   = iterateGoL   5 gol1
  let seeds2 = iterateSeeds 5 seeds1

  let gol3   = iterateWorld golStep   5 gol2
  let seeds3 = iterateWorld seedsStep 5 seeds2

  saveGoLToFile "./data/GoL/glider_10th_gen.txt" gol3
  saveToFile'' unwrapSeeds "./data/Seeds/world1_10th_gen.txt" seeds3

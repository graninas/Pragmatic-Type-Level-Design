module Main where

import Board (Board)
import Cell (Cell(..))
import Automaton
    ( Automaton(saveToFile, loadFromFile), iterateWorld )
import GameOfLife ( GoL(..) )
import Seeds ( Seeds )

import qualified Data.Map as Map

glider :: Board
glider = Map.fromList [((1, 0), Alive),
                       ((2, 1), Alive),
                       ((0, 2), Alive),
                       ((1, 2), Alive),
                       ((2, 2), Alive)]

cross :: Board
cross = error "Not implemented"

data AutomatonTag
  = GoLTag
  | SeedsTag
  | ReplicatorTag
  deriving (Show, Eq)

patterns :: [(AutomatonTag, Board)]
patterns =
  [ (GoLTag, glider)
  , (SeedsTag, cross)
  ]


main :: IO ()
main = do
  gol1   :: GoL   <- loadFromFile "./data/GoL/glider.txt"
  seeds1 :: Seeds <- loadFromFile "./data/Seeds/world1.txt"

  let gol2   = iterateWorld 5 gol1
  let seeds2 = iterateWorld 3 seeds1

  saveToFile "./data/GoL/glider_5th_gen.txt" gol2
  saveToFile "./data/Seeds/world1_3th_gen.txt" seeds2

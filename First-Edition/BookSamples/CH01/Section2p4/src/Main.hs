module Main where

import Cell (Cell(..))
import Automaton ( CellWorld(..), automatonWorldName )
import GameOfLife ( GoL )
import Seeds ( Seeds, SeedsRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))

glider :: GoL
glider = CW (Map.fromList
  [((1, 0), Alive),
  ((2, 1), Alive),
  ((0, 2), Alive),
  ((1, 2), Alive),
  ((2, 2), Alive)]
  )

main :: IO ()
main = print (automatonWorldName glider)


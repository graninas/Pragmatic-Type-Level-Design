module Main where

import Cell (Cell(..))
import Automaton ( CellWorld(..), automatonWorldName )
import GameOfLife ( GoL, GoLRule )
import Seeds ( Seeds, SeedsRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))

gameOfLifeEmptyWorld :: GoL
-- Alternative description:
-- gameOfLifeEmptyWorld :: CellWorld GoLRule
gameOfLifeEmptyWorld = CW Map.empty

main :: IO ()
main = print (automatonWorldName gameOfLifeEmptyWorld)


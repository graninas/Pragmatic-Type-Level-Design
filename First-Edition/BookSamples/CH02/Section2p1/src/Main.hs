module Main where


import Automaton
    ( automatonName,
      iterateWorld,
      loadFromFile,
      saveToFile,
      Automaton(name),
      CellWorld(CW) )
import GameOfLife ( GoL, GoLRule )
import Seeds ( Seeds )
import Replicator ( Replicator )

import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )

golWorld :: GoL
golWorld = CW Map.empty

seedsWorld :: Seeds
seedsWorld = CW Map.empty

replicatorWorld :: Replicator
replicatorWorld = CW Map.empty

-- Won't compile:

-- worlds1 :: Map.Map String (CellWorld rule)
-- worlds1 = Map.fromList
--   [ ("Game of Life", golWorld)
--   , ("Seeds", seedsWorld)
--   , ("Replicator", replicatorWorld)
--   ]

-- worlds2 :: Map.Map String (CellWorld rule)
-- worlds2 = Map.fromList [("Game of Life", golWorld), ("Seeds", seedsWorld)]

-- worlds3 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds3 = Map.fromList [("Game of Life", golWorld), ("Seeds", seedsWorld)]

-- worlds4 :: Automaton rule => Map.Map String (CellWorld rule)
-- worlds4 =
--   Map.insert "Game of Life" golWorld
--   (Map.insert "Seeds" seedsWorld Map.empty)



main :: IO ()
main = do
  gol1   :: GoL <- loadFromFile "./data/GoL/glider.txt"
  seeds1 :: Seeds <- loadFromFile "./data/Seeds/world1.txt"

  let gol2   = iterateWorld 5 gol1
  let seeds2 = iterateWorld 3 seeds1

  saveToFile "./data/GoL/glider_5th_gen.txt"   gol2
  saveToFile "./data/Seeds/world1_3th_gen.txt" seeds2

  print (name (Proxy :: Proxy GoLRule))
  print (automatonName seeds2)

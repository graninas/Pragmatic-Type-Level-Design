module Board where

import Cell ( Cell, toCell )

import qualified Data.Map as Map

type Coords = (Int, Int)
type Board = Map.Map Coords Cell

loadFromFile :: FilePath -> IO Board
loadFromFile path = do
  (content :: String) <- readFile path

  let (rows :: [String]) = lines content
  let (cells :: [[Cell]]) = map (map toCell) rows

  pure (toBoard cells)

saveToFile :: FilePath -> Board -> IO ()
saveToFile path board =
  error "not implemented"


toBoard :: [[Cell]] -> Board
toBoard cells = let
  ixedCells :: [(Int, [(Int, Cell)])] = zip [1..] (map (zip [1..]) cells)
  ixedList = foldr joinCells [] ixedCells
  in Map.fromList ixedList
  where
    joinCells
      :: (Int, [(Int, Cell)])
      -> [((Int, Int), Cell)]
      -> [((Int, Int), Cell)]
    joinCells (i, rs) lst =
      lst ++ map (\(j, cell) -> ((i, j), cell)) rs

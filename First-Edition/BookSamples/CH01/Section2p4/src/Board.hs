{-# LANGUAGE ScopedTypeVariables #-}
module Board where

import Cell ( Cell, toCell )

import qualified Data.Map as Map

type Coords = (Int, Int)
type Board = Map.Map Coords Cell

loadBoardFromFile :: FilePath -> IO Board
loadBoardFromFile path = do
  (content :: String) <- readFile path              -- ScopedTypeVariables here

  let (rows :: [String]) = lines content            -- ScopedTypeVariables here
  let (cells :: [[Cell]]) = map (map toCell) rows   -- ScopedTypeVariables here

  pure (toBoard cells)

saveBoardToFile :: FilePath -> Board -> IO ()
saveBoardToFile path board =
  error "not implemented"


toBoard :: [[Cell]] -> Board
toBoard cells = let
  ixedCells :: [(Int, [(Int, Cell)])] = zip [1..] (map (zip [1..]) cells)  -- ScopedTypeVariables here
  ixedList = foldr joinCells [] ixedCells
  in Map.fromList ixedList
  where
    joinCells
      :: (Int, [(Int, Cell)])
      -> [((Int, Int), Cell)]
      -> [((Int, Int), Cell)]
    joinCells (i, rs) lst =
      lst ++ map (\(j, cell) -> ((i, j), cell)) rs

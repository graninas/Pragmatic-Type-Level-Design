module Board where

import Cell ( toCell, Cell(..) )

import qualified Data.Map as Map

type Coords = (Int, Int)
type Board = Map.Map Coords Cell

loadBoardFromFile :: FilePath -> IO Board
loadBoardFromFile path = do
  (content :: String) <- readFile path

  let (rows :: [String]) = lines content
  let (cells :: [[Cell]]) = map (map toCell) rows

  pure (toBoard cells)

saveBoardToFile :: FilePath -> Board -> IO ()
saveBoardToFile path board =
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

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) =
    [(x-1, y-1), (x, y-1), (x+1, y-1),
     (x-1, y  ),           (x+1, y  ),
     (x-1, y+1), (x, y+1), (x+1, y+1)]

countAliveNeighbours :: Board -> (Int, Int) -> Int
countAliveNeighbours board pos = let
  ns = neighbours pos
  cells = map (`Map.lookup` board) ns
  in length $ filter (== Just Alive) cells


printBoard :: Board -> IO ()
printBoard board = do
    let (xs, ys) = unzip $ Map.keys board
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys
        printRow board coords@(_, x)
            | x == maxX = putStrLn $ cellChar $ Map.findWithDefault Dead coords board
            | otherwise = putStr $ cellChar $ Map.findWithDefault Dead coords board
        cellChar Alive = "#"
        cellChar Dead  = "."
    mapM_ (printRow board) [(y, x) | y <- [minY..maxY], x <- [minX..maxX]]


fillBoard :: (Int, Int) -> (Int, Int) -> Board -> Board
fillBoard (fromX, fromY) (toX, toY) board =
  let newCells = [(x, y)
        | x <- [fromX .. toX]
        , y <- [fromY .. toY]]
  in foldr (\k -> Map.insertWith (\_ a -> a) k Dead) board newCells

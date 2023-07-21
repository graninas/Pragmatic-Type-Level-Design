module GameOfLife where

import Board

import qualified Data.Map as Map


newtype GoL = GoL Board
  deriving (Show, Eq)

-- TODO: rules

golStep :: GoL -> GoL
golStep = error "Not implemented"

iterateGoL :: Int -> GoL -> GoL
iterateGoL n gol | n == 0 = gol
iterateGoL n gol | n > 0 =
  head (drop 5 (iterate golStep gol))
iterateGoL _ _ = error "Invalid iteration count"

loadGoLFromFile :: FilePath -> IO GoL
loadGoLFromFile path = do
  (board :: Board) <- loadFromFile path
  pure (GoL board)

saveGoLToFile :: FilePath -> GoL -> IO ()
saveGoLToFile path (GoL board) =
  saveToFile path board


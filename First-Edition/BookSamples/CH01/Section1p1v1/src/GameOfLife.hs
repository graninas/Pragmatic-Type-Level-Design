module GameOfLife where

import Board ( Board )

import qualified Data.Map as Map


-- TODO: rules

golStep :: Board -> Board
golStep = error "Not implemented"

iterateWorld :: Int -> Board -> Board
iterateWorld n board | n == 0 = board
iterateWorld n board | n > 0 =
  head (drop n (iterate golStep board))
iterateWorld _ _ = error "Invalid iteration count"


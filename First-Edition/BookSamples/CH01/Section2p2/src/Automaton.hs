module Automaton where

import Board ( Board, saveBoardToFile, loadBoardFromFile )

class Automaton ca where
  step :: ca -> ca
  wrap :: Board -> ca
  unwrap :: ca -> Board

iterateWorld :: Automaton ca => Int -> ca -> ca
iterateWorld n world | n == 0 = world
iterateWorld n world | n > 0 =
  head (drop n (iterate step world))
iterateWorld _ _ = error "Invalid iteration count"

loadFromFile :: Automaton ca => FilePath -> IO ca
loadFromFile file = do
  (board :: Board) <- loadBoardFromFile file
  pure (wrap board)

saveToFile :: Automaton ca => FilePath -> ca -> IO ()
saveToFile file world = saveBoardToFile file (unwrap world)

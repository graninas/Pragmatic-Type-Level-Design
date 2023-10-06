module Automaton where

import Board ( Board, saveBoardToFile, loadBoardFromFile )

class Automaton a where
  step :: a -> a
  wrap :: Board -> a
  unwrap :: a -> Board

iterateWorld :: Automaton a => Int -> a -> a
iterateWorld n world | n == 0 = world
iterateWorld n world | n > 0 =
  head (drop 5 (iterate step world))
iterateWorld _ _ = error "Invalid iteration count"

loadFromFile :: Automaton a => FilePath -> IO a
loadFromFile file = do
  board <- loadBoardFromFile file
  pure (wrap board)

saveToFile :: Automaton a => FilePath -> a -> IO ()
saveToFile file world = saveBoardToFile file (unwrap world)

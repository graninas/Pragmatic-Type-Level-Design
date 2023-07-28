module Automaton where


class Automaton a where
  step :: a -> a
  loadFromFile :: FilePath -> IO a
  saveToFile :: FilePath -> a -> IO ()

iterateWorld :: Automaton a => Int -> a -> a
iterateWorld n world | n == 0 = world
iterateWorld n world | n > 0 =
  head (drop 5 (iterate step world))
iterateWorld _ _ = error "Invalid iteration count"

module Minefield.Game.RndGen where

import CPrelude

import Minefield.Game.Types
import Minefield.Game.System

import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


createRandomCell
  :: [Char]
  -> (Int, Int)
  -> IO ((Int, Int), Char)
createRandomCell icons pos = do
  rndIdx <- randomRIO (0, (length icons) - 1)
  pure (pos, icons !! rndIdx)

writeRandomEmptyCells
  :: Char
  -> Float
  -> [((Int, Int), Char)]
  -> IO (Map.Map (Int, Int) Char)
writeRandomEmptyCells ecIcon percent cells = do

  let maxIdx = (length cells) - 1
  let cnt = truncate (percent * fromIntegral maxIdx)

  -- FIXME: not very reliable operation
  rndIndeces <- (L.take cnt . L.nub)
    <$> (replicateM (cnt * 3) $ randomRIO (0, maxIdx))

  let rndCells = map (\idx -> cells !! idx) rndIndeces

  let newMap1 = foldr (\(p, _) -> Map.insert p ecIcon) Map.empty rndCells
  let newMap2 = foldr maybeInsert newMap1 cells

  pure newMap2

  where
    maybeInsert (p, ch) field =
      Map.insertWith (\_ old -> old) p ch field

writeRandomPlayer
  :: (Int, Int)
  -> Char
  -> Map.Map (Int, Int) Char
  -> IO (Map.Map (Int, Int) Char)
writeRandomPlayer (w, h) ch cells = do
  x <- randomRIO (0, w - 1)
  y <- randomRIO (0, h - 1)
  pure $ Map.insert (x, y) ch cells


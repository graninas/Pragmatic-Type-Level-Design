module Minefield.Game.RndGen where

import CPrelude

import Minefield.Core.Types
import Minefield.Game.Types
import Minefield.Game.System

import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


createRandomCell
  :: [(ObjectType, Char)]
  -> (Int, Int)
  -> IO ((Int, Int), (ObjectType, Char))
createRandomCell icons pos = do
  rndIdx <- randomRIO (0, (length icons) - 1)
  pure (pos, icons !! rndIdx)

writeRandomEmptyCells
  :: (ObjectType, Char)
  -> Float
  -> [((Int, Int), (ObjectType, Char))]
  -> IO FieldObjects
writeRandomEmptyCells iconObj percent cells = do

  let maxIdx = (length cells) - 1
  let cnt = truncate (percent * fromIntegral maxIdx)

  -- FIXME: not very reliable operation
  rndIndeces <- (L.take cnt . L.nub)
    <$> (replicateM (cnt * 3) $ randomRIO (0, maxIdx))

  let rndCells = map (\idx -> cells !! idx) rndIndeces

  let newMap1 = foldr (\(p, _) -> Map.insert p iconObj) Map.empty rndCells
  let newMap2 = foldr maybeInsert newMap1 cells

  pure newMap2

  where
    maybeInsert (p, ch) field =
      Map.insertWith (\_ old -> old) p ch field

writeRandomPlayer
  :: (Int, Int)
  -> (ObjectType, Char)
  -> FieldObjects
  -> IO FieldObjects
writeRandomPlayer (w, h) obj cells = do
  x <- randomRIO (0, w - 1)
  y <- randomRIO (0, h - 1)
  pure $ Map.insert (x, y) obj cells


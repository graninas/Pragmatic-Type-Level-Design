module Minefield.Game.RndGen where

import CPrelude

import Minefield.Core.Types
import Minefield.Core.Object
import Minefield.Game.Types
import Minefield.Game.System

import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


createRandomCell
  :: [ObjectInfo]
  -> (Int, Int)
  -> IO ((Int, Int), ObjectInfo)
createRandomCell oInfos pos = do
  rndIdx <- randomRIO (0, (length oInfos) - 1)
  pure (pos, oInfos !! rndIdx)

writeRandomEmptyCells
  :: ObjectInfo
  -> Float
  -> [((Int, Int), ObjectInfo)]
  -> IO FieldObjects
writeRandomEmptyCells oInfo percent cells = do

  let maxIdx = (length cells) - 1
  let cnt = truncate (percent * fromIntegral maxIdx)

  -- FIXME: not very reliable operation
  rndIndeces <- (L.take cnt . L.nub)
    <$> (replicateM (cnt * 3) $ randomRIO (0, maxIdx))

  let rndCells = map (\idx -> cells !! idx) rndIndeces

  let newMap1 = foldr (\(p, _) -> Map.insert p oInfo) Map.empty rndCells
  let newMap2 = foldr maybeInsert newMap1 cells

  pure newMap2

  where
    maybeInsert (p, oInfo) field =
      Map.insertWith (\_ old -> old) p oInfo field

writeRandomPlayer
  :: (Int, Int)
  -> ObjectInfo
  -> FieldObjects
  -> IO FieldObjects
writeRandomPlayer (w, h) oInfo cells = do
  x <- randomRIO (0, w - 1)
  y <- randomRIO (0, h - 1)
  pure $ Map.insert (x, y) oInfo cells


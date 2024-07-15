module Minefield.Game.RndGen where

import CPrelude

import Minefield.Core.Types
import Minefield.Game.Types
import Minefield.Game.System

import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


createRandomCell
  :: [ObjectType]
  -> (Int, Int)
  -> IO ((Int, Int), ObjectType)
createRandomCell oTypes pos = do
  rndIdx <- randomRIO (0, (length oTypes) - 1)
  pure (pos, oTypes !! rndIdx)

writeRandomEmptyCells
  :: ObjectType
  -> Float
  -> [((Int, Int), ObjectType)]
  -> IO FieldObjects
writeRandomEmptyCells oType percent cells = do

  let maxIdx = (length cells) - 1
  let cnt = truncate (percent * fromIntegral maxIdx)

  -- FIXME: not very reliable operation
  rndIndeces <- (L.take cnt . L.nub)
    <$> (replicateM (cnt * 3) $ randomRIO (0, maxIdx))

  let rndCells = map (\idx -> cells !! idx) rndIndeces

  let newMap1 = foldr (\(p, _) -> Map.insert p oType) Map.empty rndCells
  let newMap2 = foldr maybeInsert newMap1 cells

  pure newMap2

  where
    maybeInsert (p, oType) field =
      Map.insertWith (\_ old -> old) p oType field

writeRandomPlayer
  :: (Int, Int)
  -> ObjectType
  -> FieldObjects
  -> IO FieldObjects
writeRandomPlayer (w, h) oType cells = do
  x <- randomRIO (0, w - 1)
  y <- randomRIO (0, h - 1)
  pure $ Map.insert (x, y) oType cells


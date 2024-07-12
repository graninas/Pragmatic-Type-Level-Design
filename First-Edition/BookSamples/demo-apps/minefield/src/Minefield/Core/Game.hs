{-# LANGUAGE AllowAmbiguousTypes #-}

module Minefield.Core.Game where

import CPrelude

import Minefield.Core.Language
import Minefield.Core.System
import Minefield.Core.Eval

import GHC.TypeLits
import qualified Data.List as L
import qualified Data.Map as Map
import System.Random (randomRIO)


data Actor = Actor
  { aThreadId :: ThreadId
  , aIcon :: Char
  }

type Field = Map.Map (Int, Int) Actor

data GameRuntime = GameRuntime
  { grFieldRef :: IORef Field

  }




createRandomGame
  :: forall g field player emptyCell objects actions
   . ( g ~ Game field player emptyCell objects actions
     , Eval GetIcon player Char
     , Eval GetIcon emptyCell Char
     , Eval GetIcon (Objects objects) [Char]
     )
  => EmptyCellsPercent
  -> (Int, Int)
  -> IO GameRuntime
createRandomGame emptyCellsPercent (w, h) = do

  let getIcon  = Proxy @GetIcon
  let pIcon    = eval getIcon $ Proxy @player
  let ecIcon   = eval getIcon $ Proxy @emptyCell
  let objIcons = eval getIcon $ Proxy @(Objects objects)

  let coords = [(x, y) | x <- [1..w], y <- [1..h]]

  cells1 <- mapM (createRandomCell objIcons) coords
  cells2 <- writeEmptyCells ecIcon emptyCellsPercent cells1


  print $ Map.toList cells2

  fieldRef <- newIORef Map.empty
  pure $ GameRuntime fieldRef

createRandomCell
  :: [Char]
  -> (Int, Int)
  -> IO ((Int, Int), Char)
createRandomCell icons pos = do
  rndIdx <- randomRIO (0, (length icons) - 1)
  pure (pos, icons !! rndIdx)


writeEmptyCells
  :: Char
  -> Float
  -> [((Int, Int), Char)]
  -> IO (Map.Map (Int, Int) Char)
writeEmptyCells ecIcon percent cells = do

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

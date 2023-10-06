{-# LANGUAGE GADTs #-}
module Domain.BoardNG where

import qualified Data.Map as Map

import GHC.TypeLits
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

type StateIdx = Int
type GenericCoords = [Int]
type Board = Map.Map GenericCoords StateIdx


data Neighborhood where
  AdjacentsLvl :: Nat -> Neighborhood


type Cells = [(GenericCoords, StateIdx)]

neighbors
  :: GenericCoords
  -> Neighborhood
  -> Board
  -> Cells
neighbors coords nsDef board = let
  ns = generateNeighborhood coords nsDef
  in getCells ns 0 board    -- TODO: default cell state

generateNeighborhood
  :: GenericCoords
  -> Neighborhood
  -> [GenericCoords]
generateNeighborhood coords (AdjacentsLvl 1)
  = filter (/= coords)
  $ mapM (\x -> [x-1, x, x+1]) coords
generateNeighborhood _ _ = error "Neighborhood not implemented for adjacents lvl > 1"

getCells
  :: [GenericCoords]
  -> StateIdx
  -> Board
  -> Cells
getCells ns def board =
  map (\coord ->
    (coord, fromMaybe def (Map.lookup coord board))) ns



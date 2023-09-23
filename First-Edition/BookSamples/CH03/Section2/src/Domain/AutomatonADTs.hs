{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Domain.AutomatonADTs where


import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Domain.Cell (Cell)


data Topology = Open | Torus
type Dimension = Int
type States = Int

type RuleCode = String
type RuleName = String

data Neighborhood = AdjacentsLvl Int

data CustomBoard =
  RegularBoard
    Topology
    Dimension
    States

data CustomRule =
  Rule
    RuleName
    RuleCode
    CustomBoard
    Neighborhood


board :: CustomBoard
board = RegularBoard Open 2 2

gameOfLife :: CustomRule
gameOfLife = Rule "GoL" "gol" board (AdjacentsLvl 1)

printBoard :: CustomBoard -> IO ()
printBoard (RegularBoard _ b n) = print b

data CellWorld (board :: CustomBoard)

class IAutomaton board where
  step
    :: CellWorld board
    -> CellWorld board

  init :: CellWorld board






-- golStep :: GoL -> GoL
-- golStep (CW board) = CW board'
--   where
--     updateCell :: Coords -> Cell
--     updateCell pos =
--         case (Map.lookup pos board, countAliveNeighbours board pos) of
--             (Just Dead, 3)  -> Alive
--             (Just Alive, 2) -> Alive
--             (Just Alive, 3) -> Alive
--             _               -> Dead
--     board' :: Board
--     board' = Map.mapWithKey (\pos _ -> updateCell pos) board

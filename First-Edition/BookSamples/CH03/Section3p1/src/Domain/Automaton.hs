{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Language.Automaton where

import Board ( Board, saveBoardToFile, loadBoardFromFile )

import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Data.Proxy ( Proxy(..) )


newtype CellWorld (param :: Symbol)     -- DataKinds + KindSignatures are used here
  = CW Board
  deriving (Show, Eq)

type RuleCode = String


class Automaton param where
  step :: CellWorld param -> CellWorld param
  code :: Proxy param -> RuleCode
  name :: Proxy param -> String


iterateWorld
  :: Automaton param
  => Int
  -> CellWorld param
  -> CellWorld param
iterateWorld n world | n == 0 = world
iterateWorld n world | n > 0 = head (drop 5 (iterate step world))
iterateWorld _ _ = error "Invalid iteration count"

loadFromFile :: Automaton param => FilePath -> IO (CellWorld param)
loadFromFile file = do
  (board :: Board) <- loadBoardFromFile file
  pure (CW board)

saveToFile :: Automaton param => FilePath -> CellWorld param -> IO ()
saveToFile file (CW world) = saveBoardToFile file world

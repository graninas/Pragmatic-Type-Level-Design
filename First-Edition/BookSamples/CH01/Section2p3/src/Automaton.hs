{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Automaton where

import Board ( Board, saveBoardToFile, loadBoardFromFile )

import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Data.Proxy ( Proxy(..) )


newtype CellWorld (rule :: Symbol)   -- DataKinds + KindSignatures are used here
  = CellWorld Board
  deriving (Show, Eq)

class KnownSymbol rule => Automaton (rule :: Symbol) where
  step :: CellWorld rule -> CellWorld rule
  name :: CellWorld rule -> String
  name _ = symbolVal (Proxy :: Proxy rule)

-- Alternative to `name`:
automatonName
  :: forall rule            -- Brings `rule` into the scope of function's body
   . KnownSymbol rule
  => CellWorld rule
  -> String
automatonName _ = symbolVal (Proxy :: Proxy rule)

iterateWorld
  :: Automaton rule
  => Int
  -> CellWorld rule
  -> CellWorld rule
iterateWorld n world | n == 0 = world
iterateWorld n world | n > 0 = head (drop 5 (iterate step world))
iterateWorld _ _ = error "Invalid iteration count"

loadFromFile :: Automaton rule => FilePath -> IO (CellWorld rule)
loadFromFile file = do
  (board :: Board) <- loadBoardFromFile file
  pure (CellWorld board)

saveToFile :: Automaton rule => FilePath -> CellWorld rule -> IO ()
saveToFile file (CellWorld world) = saveBoardToFile file world

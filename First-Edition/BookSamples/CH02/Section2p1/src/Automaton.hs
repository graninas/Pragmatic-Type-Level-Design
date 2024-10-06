{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Automaton where

import Board ( Board, saveBoardToFile, loadBoardFromFile )

import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Data.Proxy ( Proxy(..) )


newtype CellWorld (rule :: Symbol)     -- DataKinds + KindSignatures are used here
  = CW Board
  deriving (Show, Eq)

type RuleCode = String

class KnownSymbol rule => Automaton (rule :: Symbol) where
  step :: CellWorld rule -> CellWorld rule
  code :: Proxy rule -> RuleCode
  name :: Proxy rule -> String
  name proxy = symbolVal proxy


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
iterateWorld n world | n > 0 = head (drop n (iterate step world))
iterateWorld _ _ = error "Invalid iteration count"

loadFromFile :: Automaton rule => FilePath -> IO (CellWorld rule)
loadFromFile file = do
  (board :: Board) <- loadBoardFromFile file
  pure (CW board)

saveToFile :: Automaton rule => FilePath -> CellWorld rule -> IO ()
saveToFile file (CW world) = saveBoardToFile file world

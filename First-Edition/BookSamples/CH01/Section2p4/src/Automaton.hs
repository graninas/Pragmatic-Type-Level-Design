{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Automaton where

import Board ( Board, saveBoardToFile, loadBoardFromFile )

import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import Data.Proxy ( Proxy(..) )


newtype CellWorld (rule :: Symbol)     -- DataKinds + KindSignatures are used here
  = CW Board
  deriving (Show, Eq)


automatonWorldName
  :: KnownSymbol rule
  => CellWorld rule
  -> String
automatonWorldName world = symbolVal world


-- N.B.: No generic mechanism for rules yet

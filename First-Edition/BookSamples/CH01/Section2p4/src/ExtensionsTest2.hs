module ExtensionsTest2 where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import Automaton (CellWorld(..))

-- No extensions required:
automatonWorldName'
  :: KnownSymbol rule
  => CellWorld rule
  -> String
automatonWorldName' world = symbolVal world

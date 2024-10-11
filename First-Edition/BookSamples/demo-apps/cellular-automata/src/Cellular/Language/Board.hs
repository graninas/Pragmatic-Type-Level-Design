module Cellular.Language.Board where

import qualified Data.Map as Map

import GHC.TypeLits
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)


type StateIdx = Int
type GenericCoords = [Int]
type Cells = [(GenericCoords, StateIdx)]
type Board = Map.Map GenericCoords StateIdx


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Cellular.Automaton where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)

import Cellular.Language.Board
import Cellular.Language.Algorithm
import Cellular.Language.Automaton

import Cellular.Implementation.Algorithm


initWorld :: CellWorld rule
initWorld = CW Map.empty

iterateWorld
  :: forall name code board neighborhood step
   . (MakeStep step, MakeNeighborhoodLookup neighborhood)
  => CellWorld ('Rule name code board neighborhood step)
  -> CellWorld ('Rule name code board neighborhood step)
iterateWorld (CW board) = let
  stepF = makeStep (Proxy @step) (Proxy @neighborhood)
  in CW (stepF board)

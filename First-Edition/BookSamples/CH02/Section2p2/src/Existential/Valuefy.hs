{-# LANGUAGE TypeApplications #-}
module Existential.Valuefy where

import Existential.Rules
import Existential.Worlds
import Board
import Automaton
import App

import qualified Valuefied.Rules as Val
import qualified Valuefied.Worlds as Val

import qualified Data.Map as Map
import Data.Proxy




valuefy :: WorldInstance -> Val.WorldInstance
valuefy (WI gen world) = valuefy' gen world

valuefy'
  :: forall rule
   . Automaton rule
  => Generation
  -> CellWorld rule
  -> Val.WorldInstance
valuefy' gen (CW board) = let
  ri = Val.toRuleImpl (Proxy @rule)
  in Val.WI ri gen board



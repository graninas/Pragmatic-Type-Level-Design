{-# LANGUAGE TypeApplications #-}
module Cellular.App.Existential.Valuefy where

import Cellular.App.Existential.Worlds ( WorldInstance(..), Generation )
import qualified Cellular.App.Valuefied.Rules as Val
import qualified Cellular.App.Valuefied.Worlds as Val
import Cellular.Domain.Automaton ( Automaton, CellWorld(..) )

import Data.Proxy ( Proxy(Proxy) )


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



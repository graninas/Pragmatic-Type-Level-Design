module Cellular.Assets.Automata.Rules where

import Cellular.Language.Automaton
import Cellular.App.Existential.Rules
import Cellular.Assets.Automata.GameOfLife
import Cellular.Assets.Automata.Seeds
import Cellular.Assets.Automata.Replicator

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri -> (getCode ri, ri))
  [ RI (Proxy :: Proxy SeedsRule)
  , RI (Proxy :: Proxy ReplicatorRule)
  , RI (Proxy :: Proxy GoLRule)
  ]

supportedRulesDict :: Rules
supportedRulesDict = Map.fromList supportedRules


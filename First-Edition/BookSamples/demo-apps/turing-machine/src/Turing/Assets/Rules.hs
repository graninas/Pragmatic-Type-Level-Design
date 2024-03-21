-- | Predefined set of rules.

module Turing.Assets.Rules where

import Turing.Machine.Language
import Turing.Machine.Interface
import Turing.Machine.Implementation.Static
import Turing.App.Storage
import Turing.Assets.BinaryIncrement
import Turing.Assets.SimpleRule

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


supportedRules :: [(String, RuleImpl)]
supportedRules = map (\ri -> (getName ri, ri))
  [ RI (Proxy @BinaryIncrement)
  , RI (Proxy @SimpleRule)
  ]

supportedRulesDict :: Rules
supportedRulesDict = Map.fromList supportedRules


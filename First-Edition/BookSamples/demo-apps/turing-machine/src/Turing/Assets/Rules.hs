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


supportedRules :: [(RuleIndex, RuleImpl)]
supportedRules =
  [ (0, RI (Proxy @BinaryIncrement))
  , (1, RI (Proxy @SimpleRule))
  ]

supportedRulesDict :: Rules
supportedRulesDict = Map.fromList supportedRules


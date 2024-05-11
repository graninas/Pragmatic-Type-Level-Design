-- | Predefined set of rules.

module Turing.Assets.Rules where

import Turing.Machine.Language
import Turing.Machine.Language.Materialization
import Turing.App.Storage
import Turing.Machine.Implementation.FreeMonad.Dynamic
import Turing.Machine.Implementation.FreeMonad.Static
import Turing.Assets.BinaryIncrement
import Turing.Assets.SimpleRule

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


supportedRules :: [(RuleIndex, (RuleImpl, String))]
supportedRules =
  [ (0, (TypeClassRI (Proxy @BinaryIncrement), "static, type class"))
  , (1, (TypeClassDynRI (mat () $ Proxy @BinaryIncrement), "materialized/dynamic, type class"))
  , (3,
      (FreeMonadRI $ staticRuleInterpreter (Proxy @BinaryIncrement)
      , "static, free monad"
      )
    )
  , (4,
      (FreeMonadRI $ dynamicRuleInterpreter $ mat () $ Proxy @BinaryIncrement
      , "materialized/dynamic, free monad"
      )
    )
  ]

supportedRulesDict :: Rules
supportedRulesDict = Map.fromList supportedRules

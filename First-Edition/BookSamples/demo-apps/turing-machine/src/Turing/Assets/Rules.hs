-- | Predefined set of rules.

module Turing.Assets.Rules where

import Turing.Machine.Language
import Turing.Machine.Language.Materialization
import Turing.App.Storage
import Turing.Machine.Implementation.Dynamic
import Turing.Machine.Implementation.FreeMonad
import Turing.Assets.BinaryIncrement
import Turing.Assets.SimpleRule

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


supportedRules :: [(RuleIndex, (RuleImpl, String))]
supportedRules =
  [ (0, (TypeClassRI (Proxy @BinaryIncrement), "static, type class"))
  , (1, (TypeClassRI (Proxy @SimpleRule), "static, type class"))
  , (2,
      (FreeMonadRI $ ruleInterpreter $ mat () $ Proxy @BinaryIncrement
      , "materialized, free monad"
      )
    )
  , (3,
      (FreeMonadRI $ ruleInterpreter $ mat () $ Proxy @SimpleRule
      , "materialized, free monad"
      )
    )
  ]

supportedRulesDict :: Rules
supportedRulesDict = Map.fromList supportedRules

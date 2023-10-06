{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Valuefied.Rules where

import Board
import Automaton
import GameOfLife ( GoLRule )
import Seeds ( SeedsRule )
import Replicator ( ReplicatorRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


data RuleImpl = RuleImpl
  { ruleName :: String
  , ruleCode :: RuleCode
  , ruleLoad :: FilePath -> IO Board
  , ruleStep :: Board -> Board
  }

supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri@(RuleImpl _ ruleCode _ _) -> (ruleCode, ri))
  [ toRuleImpl (Proxy :: Proxy SeedsRule)
  , toRuleImpl (Proxy :: Proxy ReplicatorRule)
  , toRuleImpl (Proxy :: Proxy GoLRule)
  ]
  where
    toRuleImpl :: Automaton rule => Proxy rule -> RuleImpl
    toRuleImpl proxy =
      RuleImpl (name proxy) (code proxy) loadBoardFromFile (toStep' proxy)

    toStep'
      :: forall rule
      . Automaton rule
      => Proxy rule
      -> (Board -> Board)
    toStep' _ board =
      let (CW board') = step @rule (CW board)
      in board'


supportedRulesDict :: Map.Map RuleCode RuleImpl
supportedRulesDict = Map.fromList supportedRules

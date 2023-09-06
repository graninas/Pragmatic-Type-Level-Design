{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Valuefied.Rules where

import Board ( loadBoardFromFile, Board )
import Automaton ( Automaton(..), RuleCode, CellWorld(CW) )
import Automata.GameOfLife ( GoLRule )
import Automata.Seeds ( SeedsRule )
import Automata.Replicator ( ReplicatorRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


data RuleImpl = RuleImpl
  { ruleName :: String
  , ruleCode :: RuleCode
  , ruleLoad :: FilePath -> IO Board
  , ruleStep :: Board -> Board
  }



supportedRules :: [(RuleCode, RuleImpl)]
supportedRules = map (\ri -> (ruleCode ri, ri)) supportedRules'

supportedRules' :: [RuleImpl]
supportedRules' =
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

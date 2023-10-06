{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module App.Valuefied.Rules where

import Domain.Board ( loadBoardFromFile, Board )
import Domain.Automaton ( Automaton(..), RuleCode, CellWorld(CW) )
import Assets.Automata.GameOfLife ( GoLRule )
import Assets.Automata.Seeds ( SeedsRule )
import Assets.Automata.Replicator ( ReplicatorRule )

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


data RuleImpl = RI
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

toRuleImpl :: Automaton rule => Proxy rule -> RuleImpl
toRuleImpl proxy =
  RI (name proxy) (code proxy) loadBoardFromFile (toStep' proxy)

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

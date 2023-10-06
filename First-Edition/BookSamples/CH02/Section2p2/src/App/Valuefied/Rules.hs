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
  RI (name proxy) (code proxy) loadBoardFromFile (valuefyStep proxy)

valuefyStep
  :: forall rule
  . Automaton rule
  => Proxy rule
  -> (Board -> Board)
valuefyStep _ board1 = let
    cw :: CellWorld rule = step (CW board1)
    CW board2 = cw
  in board2


supportedRulesDict :: Map.Map RuleCode RuleImpl
supportedRulesDict = Map.fromList supportedRules

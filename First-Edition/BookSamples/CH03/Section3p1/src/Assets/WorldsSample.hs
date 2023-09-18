{-# LANGUAGE TypeApplications #-}
module Assets.WorldsSample where

import App.Existential.Rules ( RuleImpl(RI), supportedRules )
import App.Existential.Worlds ( WorldInstance(..) )
import Domain.Automaton
    ( Automaton, RuleCode, CellWorld(..), loadFromFile )

import Assets.Automata.GameOfLife ( GoL )
import Assets.Automata.Seeds ( Seeds )
import Assets.Automata.Replicator ( Replicator )

import qualified Data.Map as Map
import Data.Proxy ( Proxy )


golWorld :: GoL
golWorld = CW Map.empty

seedsWorld :: Seeds
seedsWorld = CW Map.empty

replicatorWorld :: Replicator
replicatorWorld = CW Map.empty


worlds :: Map.Map String WorldInstance
worlds = Map.fromList
  [ ("gol",   mkWorld golWorld)
  , ("seeds", mkWorld seedsWorld)
  , ("repl",  mkWorld replicatorWorld)
  ]

mkWorld
  :: forall rule
   . Automaton rule
  => CellWorld rule
  -> WorldInstance
mkWorld cw = WI 0 cw



loadWorld' :: RuleCode -> FilePath -> IO WorldInstance
loadWorld' ruleCode path = do
  case lookup ruleCode supportedRules of
    Nothing         -> error "Unknown rule."
    Just (RI proxy) -> loadWorld'' proxy path

loadWorld''
  :: forall rule        -- Brings `rule` into the scope of body
   . Automaton rule     -- Demands the `rule` to be automaton.
  => Proxy rule         -- Highlights what rule type was requrested by the caller.
  -> FilePath
  -> IO WorldInstance
loadWorld'' _ path = do
  world <- loadFromFile @rule path
  pure (WI 0 world)    -- specifying the automaton rule

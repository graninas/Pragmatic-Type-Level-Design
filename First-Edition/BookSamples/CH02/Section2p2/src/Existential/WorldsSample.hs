{-# LANGUAGE TypeApplications #-}
module Existential.WorldsSample where

import Existential.Rules
import Existential.Worlds
import Automaton

import Automata.GameOfLife
import Automata.Seeds
import Automata.Replicator

import qualified Data.Map as Map
import Data.Proxy
import Data.IORef



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
mkWorld cw = WI (RI (Proxy @rule)) 0 cw





loadWorld' :: IORef Worlds -> RuleCode -> FilePath -> IO ()
loadWorld' worldsRef ruleCode path = do
  case Map.lookup ruleCode supportedRulesDict of
    Nothing         -> putStrLn "Unknown rule."
    Just (RI proxy) -> loadWorld'' worldsRef proxy path    -- unpacks @rule


loadWorld''
  :: forall rule        -- Brings `rule` into the scope of body
   . Automaton rule     -- Demands the `rule` to be automaton.
  => IORef Worlds
  -> Proxy rule         -- Highlights what rule type was requrested by the caller
  -> FilePath
  -> IO ()
loadWorld'' worldsRef proxy path = do
  world <- loadFromFile path

  worlds <- readIORef worldsRef

  let w = WI @rule (RI proxy) 0 world    -- specifying the automaton rule

  let idx = Map.size worlds
  let worlds' = Map.insert idx w worlds
  writeIORef worldsRef worlds'

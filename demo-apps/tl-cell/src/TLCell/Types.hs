module TLCell.Types where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Set as Set


data RuleTag a



data Automaton' (rule :: RuleTag brt)



type family MkRule (a :: *) :: RuleTag a


type Automaton rule = Automaton' rule                  -- just synonym

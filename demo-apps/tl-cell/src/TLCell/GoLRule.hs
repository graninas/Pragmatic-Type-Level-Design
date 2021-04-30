module TLCell.GoLRule where

import CPrelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import TLCell.Types



data GoLRule'



type GoLAutomaton = Automaton (MkRule GoLRule')

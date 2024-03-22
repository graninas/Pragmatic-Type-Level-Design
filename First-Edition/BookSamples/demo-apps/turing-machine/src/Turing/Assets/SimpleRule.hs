{-# LANGUAGE DataKinds #-}

module Turing.Assets.SimpleRule where

import Turing.Machine.Language

import Lib.TypeSelector

-- Simple Turing Machine rule.
-- If "A", changes "A" to "B".
-- Moves left.
-- If "B", changes "B" to "C".
-- Finishes on the last cell.

-- "BA"  | "CB"
--   ^   |  ^

-- "BB"  | "CB"
--   ^   |  ^

-- "CB"  | "CB"
--   ^   |  ^

-- "BA"  | "_BA"
--  ^    |  ^

-- "AB"  | "_BB"
--  ^    |  ^

type SimpleRule = 'Rule @TypeLevel "Simple rule" 0
  '[ 'State 0 "0"
      '[ 'Match "A" ('Write "B") 'L 1
       , 'MatchAny 'WriteMatched 'L 1
       ]
   , 'State 1 "1"
      '[ 'Match "B" ('Write "C") 'Stay 2
       , 'MatchAny 'WriteMatched 'Stay 2
       ]
   , 'FinishState 2 "End"
   ]

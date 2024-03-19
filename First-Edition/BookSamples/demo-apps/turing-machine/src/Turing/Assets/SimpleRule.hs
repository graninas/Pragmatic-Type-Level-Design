{-# LANGUAGE DataKinds #-}

module Turing.Assets.SimpleRule where

import Turing.Machine.Language





type SimpleRule = 'Rule "Simple rule" 0
  '[ 'State 0 "0"
      '[ 'Match "A" ('Write "B") 'L 1
       , 'AnyMatch 'WriteMatched 'L 1
       ]
   , 'State 1 "1"
      '[ 'Match "B" ('Write "C") 'Stay 2
       , 'AnyMatch 'WriteMatched 'Stay 2
       ]
   , 'FinishState 2 "End"
   ]

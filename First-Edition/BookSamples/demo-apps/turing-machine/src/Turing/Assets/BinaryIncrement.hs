{-# LANGUAGE DataKinds #-}

module Turing.Assets.BinaryIncrement where

import Turing.Machine.Language

import Lib.TypeSelector

{-
Binary Increment

Turing machine designed to increment a binary number by one.

Symbols
- 0 and 1: Binary digits.
- B: Blank symbol (assuming the tape is initially filled with this symbol outside of the binary number).

States
- Start: Start state, where the machine begins execution.
- FindRightmost: Looking for the rightmost digit.
- Increment: Incrementing the number.
- Carry: State for handling the carry when a 1 is turned into a 0.
- FindLeftmost: Looking for the beginning of the number.
- Halt: The accepting state, where the machine stops execution.

Rule

1. Start

  Start: 0. Skip. Right. FindRightmost.
  Start: 1. Skip. Right. FindRightmost.
  Start: B. Invalid symbol.

2. Find the Rightmost Digit

  FindRightmost: 0. Skip. Right. FindRightmost.
  FindRightmost: 1. Skip. Right. FindRightmost.
  FindRightmost: B. Skip. Left. Increment.

3. Handle Increment

  Increment: 1. Write 0. Left. Carry.
  Increment: 0. Write 1. Left. FindLeftmost.
  Increment: B. Invalid symbol.

4. Carry the One

  Carry: 1. Write 0. Left. Carry.
  Carry: 0. Write 1. Left. FindLeftmost.
  Carry: B. Write 1. Stay. Halt.

5. Find the Leftmost Digit

  FindLeftmost: 0. Write 0. Left. FindLeftmost.
  FindLeftmost: 1. Write 1. Left. FindLeftmost.
  FindLeftmost: B. Write B. Right. Halt.

6. Halt
-}

type BinaryIncrement = 'Rule @TypeLevel "Binary Increment" 1
  '[ 'State 1 "Start"
      '[ 'Match "0" 'Skip 'R 2
       , 'Match "1" 'Skip 'R 2
       , 'FailWith "Rule should start from a digit."
       ]
   , 'State 2 "Find the Rightmost Digit"
      '[ 'Match "0" 'Skip 'R 2
       , 'Match "1" 'Skip 'R 2
       , 'MatchBlank 'Skip 'L 3
       ]
   , 'State 3 "Handle Increment"
      '[ 'Match "1" ('Write "0") 'L 4
       , 'Match "0" ('Write "1") 'L 5
       ]
   , 'State 4 "Carry the One"
      '[ 'Match "1" ('Write "0") 'L 4
       , 'Match "0" ('Write "1") 'L 5
       , 'MatchBlank ('Write "1") 'Stay 6
       ]
   , 'State 5 "Find the Leftmost Digit"
      '[ 'Match "0" ('Write "0") 'L 5
       , 'Match "1" ('Write "1") 'L 5
       , 'MatchBlank 'Skip 'R 6
       ]
   , 'FinishState 6 "Success"
   ]

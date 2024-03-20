-- | Domain types that describe data.
module Turing.Machine.Language.Tape where

import GHC.TypeLits



-- Simplest tape possible.


data Tape = Tape
  { tLeftTape :: [TapeSymbol]
  , tCurrentSymbol :: TapeSymbol
  , tRightTape :: [TapeSymbol]
  }
  deriving (Show, Eq, Ord)


type TapeSymbol = Either ServiceSymbol Char

data ServiceSymbol = Blank
  deriving (Show, Eq, Ord)



class SameTapeSymbol s1 s2 where
  sameTapeSymbol :: s1 -> s2 -> Bool

class WriteTape s where
  writeTape :: Tape -> s -> Tape

class InitTape s where
  initTape :: s -> Tape

instance InitTape String where
  initTape [] = Tape [] (Left Blank) []
  initTape (ch : s) = Tape [] (Right ch) (map Right s)

readTape :: Tape -> TapeSymbol
readTape (Tape _ curS _) = curS

isBlank :: TapeSymbol -> Bool
isBlank (Left Blank) = True
isBlank _ = False

-- Compares the tape symbol and the 1st char of the string
instance SameTapeSymbol TapeSymbol String where
  sameTapeSymbol (Left _) _ = False
  sameTapeSymbol _ [] = False
  sameTapeSymbol (Right ch1) (ch2 : _) = ch1 == ch2

-- Compares the tape symbol and the 1st char of the string (arguments swapped)
instance SameTapeSymbol String TapeSymbol where
  sameTapeSymbol _ (Left _) = False
  sameTapeSymbol [] _ = False
  sameTapeSymbol (ch2 : _) (Right ch1) = ch1 == ch2

-- Compares the tape symbol and the 1st char of the string
instance SameTapeSymbol TapeSymbol TapeSymbol where
  sameTapeSymbol c1 c2 = c1 == c2


-- Writes the 1st char of the string.
-- If the string is empty, writes blank.
instance WriteTape String where
  writeTape (Tape l _ r) s = Tape l (toTapeSymbol s) r

-- Writes the char.
instance WriteTape Char where
  writeTape (Tape l _ r) ch = Tape l (Right ch) r

-- Writes the service Symbol.
instance WriteTape ServiceSymbol where
  writeTape (Tape l _ r) s = Tape l (Left s) r

-- Writes the tape Symbol.
instance WriteTape TapeSymbol where
  writeTape (Tape l _ r) s = Tape l s r


shiftHeadLeft :: Tape -> Tape
shiftHeadLeft (Tape [] s rs)     = Tape [] (Left Blank) (s : rs)
shiftHeadLeft (Tape (l:ls) s rs) = Tape ls l (s : rs)

shiftHeadRight :: Tape -> Tape
shiftHeadRight (Tape ls s [])       = Tape (s : ls) (Left Blank) []
shiftHeadRight (Tape ls s (r : rs)) = Tape (s : ls) r rs

moveHeadLeft :: Tape -> Int -> Tape
moveHeadLeft tape n | n <= 0 = tape
moveHeadLeft tape n = moveHeadLeft (shiftHeadLeft tape) $ n - 1

moveHeadRight :: Tape -> Int -> Tape
moveHeadRight tape n | n <= 0 = tape
moveHeadRight tape n = moveHeadRight (shiftHeadRight tape) $ n - 1

-- | Converts the 1st symbol of the string to the type symbol.
-- Converts to blank if the string is empty.
toTapeSymbol :: String -> TapeSymbol
toTapeSymbol [] = Left Blank
toTapeSymbol (ch : _) = Right ch


shrinkBlanks :: Tape -> Tape
shrinkBlanks (Tape l cur r) = let
  l' = reverse $ dropWhile isBlank $ reverse l
  r' = reverse $ dropWhile isBlank $ reverse r
  in Tape l' cur r'

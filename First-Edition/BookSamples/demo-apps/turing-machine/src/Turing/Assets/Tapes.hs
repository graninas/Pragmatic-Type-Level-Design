-- | Predefined set of tapes.

module Turing.Assets.Tapes where

import Turing.Machine.Language

import qualified Data.Map as Map
import Data.Proxy (Proxy(..))


binaryNumberTape :: (String, Tape)
binaryNumberTape = ("Binary number", initTape @String "10011011")

abString :: (String, Tape)
abString = ("AB string", initTape @String "AB")

predefinedTapes :: [(String, Tape)]
predefinedTapes = [binaryNumberTape, abString]


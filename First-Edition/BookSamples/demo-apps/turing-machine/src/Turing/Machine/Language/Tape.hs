-- | Domain types that describe data.
module Turing.Machine.Language.Tape where

import GHC.TypeLits




data Tape = Tape String
  deriving (Show, Eq, Ord)

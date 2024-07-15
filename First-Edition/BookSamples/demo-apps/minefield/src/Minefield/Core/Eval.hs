module Minefield.Core.Eval where

import CPrelude


class Eval payload tag item ret
  | tag item -> ret where
  eval :: payload -> Proxy tag -> Proxy item -> IO ret

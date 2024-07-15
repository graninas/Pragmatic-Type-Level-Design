module Minefield.Core.Eval where

import CPrelude


class Eval tag payload ret
  | tag payload -> ret where
  eval :: Proxy tag -> Proxy payload -> IO ret

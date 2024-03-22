module Turing.Machine.Implementation.Common where

import Turing.Machine.Language


type StateIdx = Int
type CurrentStateIdx = StateIdx

data Result
  = FailedWith String
  | Finished Tape
  | Successful StateIdx Tape

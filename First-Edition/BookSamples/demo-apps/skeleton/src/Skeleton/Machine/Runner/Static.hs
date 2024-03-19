{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Skeleton.Machine.Runner.Static where

import Skeleton.Machine.Language

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Type.Equality
import Control.Monad (mapM)



class RuleRunner (rule :: CustomRule) where
  runRule
    :: Proxy rule
    -> Int          -- ^ Max number of steps
    -> Tape
    -> Tape


instance RuleRunner ('Rule ruleName curState '[]) where
  runRule _ _ tape = tape

instance
  ( (curState == stIdx) ~ 'True
  ) =>
  RuleRunner
    ('Rule
      ruleName
      curState
      ('State stIdx stName conds ': states)
    ) where
  runRule _ n tape | n <= 0 = tape
  runRule _ n tape = let

    in tape   --- TODO



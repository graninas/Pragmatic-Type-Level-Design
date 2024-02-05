{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cellular.Language.Integrity where

import GHC.TypeLits
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad (mapM)
import Data.Type.Equality

import Common.NatList


import Cellular.Language.Board
import Cellular.Language.Algorithm


-- | Integrity verification interface
class Verify tag where

-- | Helper verification machinery
class Check tag where

------- Empty states list verification
data StatesNotEmpty (states :: [CustomState])

-- No instance for empty state:
-- instance Verify (StatesNotEmpty '[]) where
--   verify _ = False

instance Verify (StatesNotEmpty (s ': ss)) where


------- At least two states verification
data AtLeastTwoStates (states :: [CustomState])

-- No instances for single state and empty state:
-- instance Verify (AtLeastTwoStates '[]) where
--   verify _ = True
-- instance Verify (AtLeastTwoStates (s1 ': ss)) where
--   verify _ = True

instance Verify (AtLeastTwoStates (s1 ': s2 ': ss)) where


------- States uniqueness verification
--  (no name check)
data StatesAreUnique (states :: [CustomState])
data StatesAreUniqueCheck
  (verified :: [CustomState])
  (toVerify :: [CustomState])

-- Aux checks
data StateNotInList s (l :: ss)

instance Check (StateNotInList s '[]) where

instance
  ( (s1 == s2) ~ 'False
  , Check (StateNotInList ('State n1 s1) ss)
  ) => Check (StateNotInList ('State n1 s1) (('State n2 s2) ': ss)) where

instance Verify (StatesAreUnique '[]) where

instance Verify (StatesAreUnique (s1 ': '[])) where

instance
  ( Check (StatesAreUniqueCheck '[s1] (s2 ': ss))
  ) => Verify (StatesAreUnique (s1 ': s2 ': ss)) where

instance
  Check (StatesAreUniqueCheck checked '[]) where

instance
  ( Check (StateNotInList s1 checked)
  , Check (StatesAreUniqueCheck (s1 ': checked) ss2)
  ) =>
  Check (StatesAreUniqueCheck checked (s1 ': ss2)) where


------- State names uniqueness verification
--  (no state check)
data StateNamesAreUnique (states :: [CustomState])
data StateNamesAreUniqueCheck
  (verified :: [CustomState])
  (toVerify :: [CustomState])

-- Aux checks
data StateNameNotInList st (l :: ss)

instance Check (StateNameNotInList st '[]) where

instance
  ( (n1 == n2) ~ 'False
  , Check (StateNameNotInList ('State n1 s1) ss)
  ) => Check (StateNameNotInList ('State n1 s1) (('State n2 s2) ': ss)) where

instance Verify (StateNamesAreUnique '[]) where

instance Verify (StateNamesAreUnique (s1 ': '[])) where

instance
  ( Check (StateNamesAreUniqueCheck '[s1] (s2 ': ss))
  ) => Verify (StateNamesAreUnique (s1 ': s2 ': ss)) where

instance
  Check (StateNamesAreUniqueCheck checked '[]) where

instance
  ( Check (StateNameNotInList s1 checked)
  , Check (StateNamesAreUniqueCheck (s1 ': checked) ss2)
  ) =>
  Check (StateNamesAreUniqueCheck checked (s1 ': ss2)) where


------- State is real verification
data StateIsReal
  (s :: StateIdxNat)
  (ss :: [CustomState])

instance
  ( StateIdxInList s ss ~ 'True
  ) =>
  Verify (StateIsReal s ss) where

type family StateIdxInList (s :: StateIdxNat) (ss :: [CustomState]) :: Bool where
    StateIdxInList _ '[] = 'False
    StateIdxInList s ('State n s ': _) = 'True
    StateIdxInList s (_ ': ss) = StateIdxInList s ss

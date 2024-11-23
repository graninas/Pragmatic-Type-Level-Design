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
    -- Nothing here; empty type class

------- Empty states list verification
data StatesNotEmpty (states :: [CustomState])

-- No instance for empty state:
-- instance Verify (StatesNotEmpty '[]) where

instance Verify (StatesNotEmpty (s ': ss)) where


------- At least two states verification
data AtLeastTwoStates (states :: [CustomState])

-- No instances for single state and empty state:
-- instance Verify (AtLeastTwoStates '[]) where
-- instance Verify (AtLeastTwoStates (s1 ': ss)) where

instance Verify (AtLeastTwoStates (s1 ': s2 ': ss)) where


------- States uniqueness verification
--  (no name check)
data StatesAreUnique (states :: [CustomState])
data StatesAreUniqueCheck
  (verified :: [CustomState])
  (toVerify :: [CustomState])

-- Aux checks
data StateNotInList s (l :: ss)

instance Verify (StateNotInList s '[]) where

instance
  ( (s1 == s2) ~ 'False
  , Verify (StateNotInList ('State n1 s1) ss)
  ) => Verify (StateNotInList ('State n1 s1) (('State n2 s2) ': ss)) where

instance Verify (StatesAreUnique '[]) where

instance Verify (StatesAreUnique (s1 ': '[])) where

instance
  ( Verify (StatesAreUniqueCheck '[s1] (s2 ': ss))
  ) => Verify (StatesAreUnique (s1 ': s2 ': ss)) where

instance
  Verify (StatesAreUniqueCheck checked '[]) where

instance
  ( Verify (StateNotInList s1 checked)
  , Verify (StatesAreUniqueCheck (s1 ': checked) ss)
  ) =>
  Verify (StatesAreUniqueCheck checked (s1 ': ss)) where


------- State names uniqueness verification
--  (no state check)
data StateNamesAreUnique (states :: [CustomState])
data StateNamesAreUniqueCheck
  (verified :: [CustomState])
  (toVerify :: [CustomState])

-- Aux checks
data StateNameNotInList st (l :: ss)

instance Verify (StateNameNotInList st '[]) where

instance
  ( (n1 == n2) ~ 'False
  , Verify (StateNameNotInList ('State n1 s1) ss)
  ) => Verify (StateNameNotInList ('State n1 s1) (('State n2 s2) ': ss)) where

instance Verify (StateNamesAreUnique '[]) where

instance Verify (StateNamesAreUnique (s1 ': '[])) where

instance
  ( Verify (StateNamesAreUniqueCheck '[s1] (s2 ': ss))
  ) => Verify (StateNamesAreUnique (s1 ': s2 ': ss)) where

instance
  Verify (StateNamesAreUniqueCheck checked '[]) where

instance
  ( Verify (StateNameNotInList s1 checked)
  , Verify (StateNamesAreUniqueCheck (s1 ': checked) ss)
  ) =>
  Verify (StateNamesAreUniqueCheck checked (s1 ': ss)) where


------- State is real verification
data StateIsReal
  (s :: StateIdxNat)
  (ss :: [CustomState])

instance
  ( StateIdxInList s ss ~ 'True
  ) =>
  Verify (StateIsReal s ss) where

type family
  StateIdxInList (s :: StateIdxNat) (ss :: [CustomState])
  :: Bool where

  StateIdxInList _ '[] = 'False
  StateIdxInList s ('State n s ': _) = 'True
  StateIdxInList s (_ ': ss) = StateIdxInList s ss


------- Default state is real verification
data DefaultStateIsReal
  (d :: DefaultState)
  (ss :: [CustomState])

instance
  ( Verify (StateIsReal defIdx ss)                         -- FlexibleContexts
  ) =>
  Verify (DefaultStateIsReal ('DefState defIdx) ss) where  -- FlexibleInstances


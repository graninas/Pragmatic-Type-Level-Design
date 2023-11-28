{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
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

-- N.B., classes can be method-less


-- Integrity verification interface
class Verify tag where
  verify :: Proxy tag -> Bool

-- Helper verification machinery
class Check tag where

-- Aux checks
data NotInList it (l :: its)

instance Check (NotInList it '[]) where

instance
  ( (it1 == it2) ~ 'False
  , Check (NotInList it1 its)
  ) => Check (NotInList it1 (it2 ': its)) where

-- Empty states list verification
data StatesNotEmpty (states :: [CustomState])

-- No instance for empty state:
-- instance Verify (StatesNotEmpty '[]) where
--   verify _ = False

instance Verify (StatesNotEmpty (s ': ss)) where
  verify _ = True


-- At least two states verification
data AtLeastTwoStates (states :: [CustomState])

-- No instances for single state and empty state:
-- instance Verify (AtLeastTwoStates '[]) where
--   verify _ = True
-- instance Verify (AtLeastTwoStates (s1 ': ss)) where
--   verify _ = True

instance Verify (AtLeastTwoStates (s1 ': s2 ': ss)) where
  verify _ = True


-- States uniqueness verification
data StatesAreUnique (states :: [CustomState])
data StatesAreUniqueCheck
  (verified :: [CustomState])
  (toVerify :: [CustomState])

instance Verify (StatesAreUnique '[]) where
  verify _ = True

instance Verify (StatesAreUnique (s1 ': '[])) where
  verify _ = True

instance
  ( Check (StatesAreUniqueCheck '[s1] (s2 ': ss))
  ) => Verify (StatesAreUnique (s1 ': s2 ': ss)) where
  verify _ = True

instance
  Check (StatesAreUniqueCheck ss '[]) where

instance
  ( Check (NotInList s1 checked)
  , Check (StatesAreUniqueCheck (s1 ': checked) ss2)
  ) =>
  Check (StatesAreUniqueCheck checked (s1 ': ss2)) where



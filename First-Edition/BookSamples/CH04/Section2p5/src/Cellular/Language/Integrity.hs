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
import Data.Type.Equality
import qualified Data.Map as Map
import Control.Monad (mapM)

import Common.NatList
import Common.NonEmptyList

import Cellular.Language.Board
import Cellular.Language.Algorithm


-- | Integrity verification interface
class Verify tag where
    -- Nothing here; empty type class

-- | Helper verification machinery
class Check tag where
    -- Nothing here; empty type class

------- States uniqueness verification
--  (no name check)
data StatesAreUnique (states :: CustomList2 CustomState)
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

instance
  ( (s1 == s2) ~ 'False
  ) => Verify (StatesAreUnique
                ('List2
                 ('State n1 s1)
                 ('State n2 s2)
                 '[])) where

instance
  ( (s1 == s2) ~ 'False
  , (s2 == s3) ~ 'False
  , s1' ~ ('State n1 s1)
  , s2' ~ ('State n2 s2)
  , s3' ~ ('State n3 s3)
  , Check (StatesAreUniqueCheck '[s1', s2', s3'] ss)
  ) => Verify (StatesAreUnique
                ('List2
                 ('State n1 s1)
                 ('State n2 s2)
                 ('State n3 s3 ': ss)
                 )) where

instance
  Check (StatesAreUniqueCheck checked '[]) where

instance
  ( Check (StateNotInList s1 checked)
  , Check (StatesAreUniqueCheck (s1 ': checked) ss2)
  ) =>
  Check (StatesAreUniqueCheck checked (s1 ': ss2)) where


-- ------- State names uniqueness verification
-- --  (no state check)
data StateNamesAreUnique (states :: CustomList2 CustomState)
data StateNamesAreUniqueCheck
  (verified :: [CustomState])
  (toVerify :: [CustomState])

-- Aux checks
data StateNameNotInList s (l :: ss)

instance Check (StateNameNotInList s '[]) where

instance
  ( (n1 == n2) ~ 'False
  , Check (StateNameNotInList ('State n1 s1) ss)
  ) => Check (StateNameNotInList ('State n1 s1) (('State n2 s2) ': ss)) where

instance
  ( (n1 == n2) ~ 'False
  ) => Verify (StateNamesAreUnique
                ('List2
                 ('State n1 s1)
                 ('State n2 s2)
                 '[])) where

instance
  ( (n1 == n2) ~ 'False
  , (n2 == n3) ~ 'False
  , s1' ~ ('State n1 s1)
  , s2' ~ ('State n2 s2)
  , s3' ~ ('State n3 s3)
  , Check (StateNamesAreUniqueCheck '[s1', s2', s3'] ss)
  ) => Verify (StateNamesAreUnique
                ('List2
                 ('State n1 s1)
                 ('State n2 s2)
                 ('State n3 s3 ': ss)
                 )) where

instance
  Check (StateNamesAreUniqueCheck checked '[]) where

instance
  ( Check (StateNameNotInList s1 checked)
  , Check (StateNamesAreUniqueCheck (s1 ': checked) ss2)
  ) =>
  Check (StateNamesAreUniqueCheck checked (s1 ': ss2)) where


-- ------- State is real verification
data StateIsReal
  (s :: StateIdxNat)
  (ss :: CustomList2 CustomState)

instance
  ( StateIdxInList2 s ss ~ 'True
  ) =>
  Verify (StateIsReal s ss) where

type family StateIdxInList
  (s :: StateIdxNat)
  (ss :: [CustomState]) :: Bool where
    StateIdxInList s ('State n s ': _) = 'True
    StateIdxInList s (_ ': ss) = StateIdxInList s ss
    StateIdxInList _ _ = 'False

type family StateIdxInList2
  (s :: StateIdxNat)
  (ss :: CustomList2 CustomState) :: Bool where
    StateIdxInList2 s ('List2 ('State _ s) _ _) = 'True
    StateIdxInList2 s ('List2 _ ('State _ s) _) = 'True
    StateIdxInList2 s ('List2 _ _ ss) = StateIdxInList s ss

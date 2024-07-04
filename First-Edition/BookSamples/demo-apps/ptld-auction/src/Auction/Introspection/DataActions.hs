{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module Auction.Introspection.DataActions where

import Auction.Introspection.Action
import Auction.Language
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- Specific actions

instance Eval AsIntroAction (GetPayloadValueImpl valName valType lam) (IO [String]) where
  eval _ _ = pure ["GetPayloadValueImpl reached"]

instance Eval AsIntroAction (GetLotNameImpl lam) (IO [String]) where
  eval _ _ = pure ["GetLotNameImpl reached"]

instance Eval AsIntroAction (GetLotDescrImpl lam) (IO [String]) where
  eval _ _ = pure ["GetLotDescrImpl reached"]

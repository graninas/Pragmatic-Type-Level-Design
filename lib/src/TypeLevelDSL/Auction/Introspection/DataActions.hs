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

module TypeLevelDSL.Auction.Introspection.DataActions where

import TypeLevelDSL.Auction.Introspection.Action
import qualified TypeLevelDSL.Auction.Language as L
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- Specific actions

instance Eval AsIntroAction (L.GetPayloadValue' valName valType lam) (IO [String]) where
  eval _ _ = pure ["GetPayloadValue' reached"]

instance Eval AsIntroAction (L.GetLotName' lam) (IO [String]) where
  eval _ _ = pure ["GetLotName' reached"]

instance Eval AsIntroAction (L.GetLotDescr' lam) (IO [String]) where
  eval _ _ = pure ["GetLotDescr' reached"]

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

import Auction.Language
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsIntroAction = AsIntroAction

instance Eval () AsIntroAction '[] (IO [String]) where
  eval () _ _ = pure []

instance
  ( Eval () AsIntroAction act (IO [String])
  , Eval () AsIntroAction acts (IO [String])
  ) =>
  Eval () AsIntroAction
    (ActionWrapper act ': acts)
    (IO [String]) where
  eval () _ _ = do
    strs1 <- eval () AsIntroAction $ Proxy @act
    strs2 <- eval () AsIntroAction $ Proxy @acts
    pure $ strs1 <> strs2

-- Specific actions

instance Eval () AsIntroAction (GetPayloadValueImpl valName valType lam) (IO [String]) where
  eval () _ _ = pure ["GetPayloadValueImpl reached"]

instance Eval () AsIntroAction (GetLotNameImpl lam) (IO [String]) where
  eval () _ _ = pure ["GetLotNameImpl reached"]

instance Eval () AsIntroAction (GetLotDescrImpl lam) (IO [String]) where
  eval () _ _ = pure ["GetLotDescrImpl reached"]

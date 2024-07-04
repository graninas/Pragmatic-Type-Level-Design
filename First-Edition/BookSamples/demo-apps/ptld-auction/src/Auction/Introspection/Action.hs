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

module Auction.Introspection.Action where

import Data.HList.HList
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsIntroAction = AsIntroAction

instance
  ( mkAct ~ MkHList act
  , Eval AsIntroAction act (IO [String])
  ) =>
  Eval AsIntroAction mkAct (IO [String]) where
  eval _ _ = eval AsIntroAction (Proxy :: Proxy act)

instance Eval AsIntroAction HEmptyImpl (IO [String]) where
  eval _ _ = pure ["HEmptyImpl reached."]

instance
  ( Eval AsIntroAction act (IO [String])
  , Eval AsIntroAction acts (IO [String])
  ) =>
  Eval AsIntroAction (HListImpl act acts) (IO [String]) where
  eval _ _ = do
    strs1 <- eval AsIntroAction (Proxy :: Proxy act)
    strs2 <- eval AsIntroAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

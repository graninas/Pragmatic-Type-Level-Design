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

module TypeLevelDSL.Auction.Implementation.Action where

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

data AsImplAction = AsImplAction

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance
  ( mkAct ~ L.MkAction act
  , Eval AsImplAction act [String]
  ) =>
  Eval AsImplAction mkAct [String] where
  eval _ _ = eval AsImplAction (Proxy :: Proxy act)

instance Eval AsImplAction L.End' [String] where
  eval _ _ = pure ["End' reached."]

instance
  ( Eval AsImplAction act [String]
  , Eval AsImplAction acts [String]
  ) =>
  Eval AsImplAction (L.Action' act acts) [String] where
  eval _ _ = do
    strs1 <- eval AsImplAction (Proxy :: Proxy act)
    strs2 <- eval AsImplAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

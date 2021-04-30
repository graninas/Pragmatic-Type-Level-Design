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

import TypeLevelDSL.Language.Action
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

data AsIntroAction = AsIntroAction

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance
  ( mkAct ~ MkAction act
  , Eval AsIntroAction act (IO [String])
  ) =>
  Eval AsIntroAction mkAct (IO [String]) where
  eval _ _ = eval AsIntroAction (Proxy :: Proxy act)

instance Eval AsIntroAction End' (IO [String]) where
  eval _ _ = pure ["End' reached."]

instance
  ( Eval AsIntroAction act (IO [String])
  , Eval AsIntroAction acts (IO [String])
  ) =>
  Eval AsIntroAction (Action' act acts) (IO [String]) where
  eval _ _ = do
    strs1 <- eval AsIntroAction (Proxy :: Proxy act)
    strs2 <- eval AsIntroAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

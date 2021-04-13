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

module TypeLevelDSL.Auction.Introspection.Action where

import TypeLevelDSL.Auction.Language.Action
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

data AsIntroAction      = AsIntroAction

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance (mkAct ~ MkAction act, Eval AsIntroAction act [String]) =>
  Eval AsIntroAction mkAct [String] where
  eval _ _ = eval AsIntroAction (Proxy :: Proxy act)

instance Eval AsIntroAction End' [String] where
  eval _ _ = pure ["End' reached."]

instance (Eval AsIntroAction act [String], Eval AsIntroAction acts [String]) =>
  Eval AsIntroAction (Action' act acts) [String] where
  eval _ _ = do
    strs1 <- eval AsIntroAction (Proxy :: Proxy act)
    strs2 <- eval AsIntroAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

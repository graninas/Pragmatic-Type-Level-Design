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
data AsImplLambda = AsImplLambda

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance
  ( mkAct ~ L.MkAction act
  , EvalCtx ctx AsImplAction act [String]
  ) =>
  EvalCtx ctx AsImplAction mkAct [String] where
  evalCtx ctx _ _ = evalCtx ctx AsImplAction (Proxy :: Proxy act)

instance EvalCtx ctx AsImplAction L.End' [String] where
  evalCtx ctx _ _ = pure ["End' reached."]

instance
  ( EvalCtx ctx AsImplAction act [String]
  , EvalCtx ctx AsImplAction acts [String]
  ) =>
  EvalCtx ctx AsImplAction (L.Action' act acts) [String] where
  evalCtx ctx _ _ = do
    strs1 <- evalCtx ctx AsImplAction (Proxy :: Proxy act)
    strs2 <- evalCtx ctx AsImplAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Lambda mechanism


instance
  ( mkLam ~ L.MkLambda lam
  , EvalLambdaCtx ctx valType AsImplLambda lam [String]
  ) =>
  EvalLambdaCtx ctx valType AsImplLambda mkLam [String] where
  evalLambdaCtx ctx val _ _ =
    evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam)

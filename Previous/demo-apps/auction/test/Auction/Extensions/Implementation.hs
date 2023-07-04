{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Auction.Extensions.Implementation where

import qualified Auction.Types as T
import qualified Auction.Language.Description as L
import qualified Auction.Extensions.Language as ExtL
import qualified Auction.Implementation.Description as Impl
import qualified TypeLevelDSL.StateContext as StCtx
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import qualified Data.Dynamic as Dyn
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- Extensions implementation

-- Dynamic (runtime) value. For now hardcoded by can be obtained from any source.
instance Eval Impl.AsImplMoneyConst (L.DynVal' "202 min bid") (IO T.Money) where
  eval _ _ = pure 20000

-- Payload

instance Eval Impl.AsImplMoneyConst minBid (IO T.Money) =>
  Eval Impl.AsImplLotPayload (ExtL.EFLotPayload' minBid) (IO StCtx.StateContext) where
  eval _ _ = do
    stCtx <- StCtx.createStateContext
    v <- eval Impl.AsImplMoneyConst (Proxy :: Proxy minBid)
    StCtx.insertValue "minBid" (Dyn.toDyn v) stCtx
    pure stCtx

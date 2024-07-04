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
import qualified Auction.Language as L
import qualified Auction.Extensions.Language as ExtL
import Auction.Implementation.Auction
import qualified TypeLevelDSL.StateContext as StCtx
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import qualified Data.Dynamic as Dyn
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- Extensions implementation

-- Dynamic (runtime) value. For now hardcoded by can be obtained from any source.
type MinBid202 = 'L.ValNameS "202 min bid"
instance Eval AsImplMoneyConst (L.DynValImpl MinBid202) (IO T.Money) where
  eval _ _ = pure 20000

-- Payload

instance Eval AsImplMoneyConst minBid (IO T.Money) =>
  Eval AsImplLotPayload (ExtL.EFLotPayloadImpl minBid) (IO StCtx.StateContext) where
  eval _ _ = do
    stCtx <- StCtx.createStateContext
    v <- eval AsImplMoneyConst (Proxy :: Proxy minBid)
    StCtx.insertValue "minBid" (Dyn.toDyn v) stCtx
    pure stCtx

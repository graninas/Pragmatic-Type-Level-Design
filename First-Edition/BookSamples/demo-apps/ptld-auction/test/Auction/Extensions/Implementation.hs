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
import Auction.Language
import Auction.Extensions.Language
import Auction.Implementation.Auction
import qualified TypeLevelDSL.StateContext as StCtx
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import qualified Data.Dynamic as Dyn
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsImplMoneyConst = AsImplMoneyConst

-- MoneyConst

instance
  ( KnownSymbol val
  ) =>
  Eval () AsImplMoneyConst (MoneyValImpl val) (IO T.Money) where
  eval () _ _ = pure $ read $ symbolVal $ Proxy @val     -- unsafe

-- Money const wrapper

instance
  ( Eval () AsImplMoneyConst a (IO T.Money)
  ) =>
  Eval () AsImplMoneyConst (MoneyConstWrapper a) (IO T.Money) where
  eval () _ _ = eval () AsImplMoneyConst $ Proxy @a

-- Extensions implementation

-- Dynamic (runtime) value. For now hardcoded by can be obtained from any source.
type MinBid202 = 'ValNameS "202 min bid"
instance
  Eval () AsImplMoneyConst (DynValImpl MinBid202) (IO T.Money) where
  eval () _ _ = pure 20000

-- Payload

instance
  ( Eval () AsImplMoneyConst minBid (IO T.Money)
  ) =>
  Eval () AsImplLotPayload
    (EFLotPayloadImpl minBid)
    (IO StCtx.StateContext) where
  eval () _ _ = do
    stCtx <- StCtx.createStateContext
    v <- eval () AsImplMoneyConst $ Proxy @minBid
    StCtx.insertValue "minBid" (Dyn.toDyn v) stCtx
    pure stCtx

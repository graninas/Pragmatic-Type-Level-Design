{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.Extensions.Implementation where

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Description.Language as L
import qualified TypeLevelDSL.Auction.Extensions.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import qualified TypeLevelDSL.Auction.Implementation.Description as Impl
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation


-- Interpreting other extensions

-- Dynamic (runtime) value. For now hardcoded by can be obtained from any source.
instance Eval Impl.AsImplMoneyConst (L.DynVal' "202 min bid") T.Money where
  eval _ _ = pure 20000

-- Payload
instance Eval Impl.AsImplMoneyConst minBid T.Money =>
  Eval Impl.AsImplLotPayload (L.Payload' minBid) Impl.Payload where
  eval _ _ = do
    v <- eval Impl.AsImplMoneyConst (Proxy :: Proxy minBid)
    pure $ Impl.Payload v

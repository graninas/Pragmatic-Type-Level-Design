{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Auction.Implementation.Description where

import Data.Proxy (Proxy(..))
import Data.IORef (newIORef)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Language.Description as L
import TypeLevelDSL.StateContext (StateContext)
import TypeLevelDSL.Eval

-- Service types

data LotDescr = LotDescr
  { ldName           :: String
  , ldDescription    :: String
  , ldPayloadContext :: StateContext
  }

type LotDescrs = [LotDescr]


-- Interpretation tags

data AsImplInfo       = AsImplInfo
data AsImplLots       = AsImplLots
data AsImplLot        = AsImplLot
data AsImplLotPayload = AsImplLotPayload
data AsImplMoneyConst = AsImplMoneyConst

-- Interpreting of the list of lots (lots :: LotsTag a)

instance Eval AsImplLot p LotDescr =>
  Eval AsImplLots (p ': '[]) LotDescrs where
  eval _ _ = do
    lot <- eval AsImplLot (Proxy :: Proxy p)
    pure [lot]

instance
  ( Eval AsImplLot p LotDescr
  , Eval AsImplLots (x ': ps) LotDescrs
  ) =>
  Eval AsImplLots (p ': x ': ps) LotDescrs where
  eval _ _ = do
    lot  <- eval AsImplLot (Proxy :: Proxy p)
    lots <- eval AsImplLots (Proxy :: Proxy (x ': ps))
    pure $ lot : lots

instance (b ~ L.MkLots a, Eval AsImplLots a LotDescrs) =>
  Eval AsImplLots b LotDescrs where
  eval _ _ = eval AsImplLots (Proxy :: Proxy a)

-- Interpreting of a Lot

instance
  ( Eval AsImplLotPayload payload StateContext
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval AsImplLot (L.Lot' name descr payload currency censorship) LotDescr where
  eval _ _ = do
    payloadCtx <- eval AsImplLotPayload (Proxy :: Proxy payload)
    pure $ LotDescr
      { ldName           = symbolVal (Proxy :: Proxy name)
      , ldDescription    = symbolVal (Proxy :: Proxy descr)
      , ldPayloadContext = payloadCtx
      }

-- Interpreting a MoneyConst value

instance (b ~ L.MkMoneyConst a, Eval AsImplMoneyConst a T.Money) =>
  Eval AsImplMoneyConst b T.Money where
  eval _ _ = eval AsImplMoneyConst (Proxy :: Proxy a)

instance KnownSymbol val =>
 Eval AsImplMoneyConst (L.MoneyVal' val) T.Money where
  eval _ _ = pure $ read $ symbolVal (Proxy :: Proxy val)     -- unsafe

-- Interpreting a LotPayload value

instance (b ~ L.MkLotPayload a, Eval AsImplLotPayload a StateContext) =>
  Eval AsImplLotPayload b StateContext where
  eval _ _ = eval AsImplLotPayload (Proxy :: Proxy a)

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

module Auction.Implementation.Description where

import Data.Proxy (Proxy(..))
import Data.IORef (newIORef)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Auction.Types as T
import qualified Auction.Language.Description as L
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

-- Interpreting of the list of lots (lots :: ILots)

instance Eval AsImplLot p (IO LotDescr) =>
  Eval AsImplLots (p ': '[]) (IO LotDescrs) where
  eval _ _ = do
    lot <- eval AsImplLot (Proxy :: Proxy p)
    pure [lot]

instance
  ( Eval AsImplLot p (IO LotDescr)
  , Eval AsImplLots (x ': ps) (IO LotDescrs)
  ) =>
  Eval AsImplLots (p ': x ': ps) (IO LotDescrs) where
  eval _ _ = do
    lot  <- eval AsImplLot (Proxy :: Proxy p)
    lots <- eval AsImplLots (Proxy :: Proxy (x ': ps))
    pure $ lot : lots

instance
  ( b ~ L.MkLots a
  , Eval AsImplLots a (IO LotDescrs)
  ) =>
  Eval AsImplLots b (IO LotDescrs) where
  eval _ _ = eval AsImplLots (Proxy :: Proxy a)

-- Interpreting of a Lot

instance
  ( Eval AsImplLotPayload payload (IO StateContext)
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval AsImplLot (L.LotImpl name descr payload currency censorship) (IO LotDescr) where
  eval _ _ = do
    payloadCtx <- eval AsImplLotPayload (Proxy :: Proxy payload)
    pure $ LotDescr
      { ldName           = symbolVal (Proxy :: Proxy name)
      , ldDescription    = symbolVal (Proxy :: Proxy descr)
      , ldPayloadContext = payloadCtx
      }

-- Interpreting a MoneyConst value

instance
  ( b ~ L.MkMoneyConst a
  , Eval AsImplMoneyConst a (IO T.Money)
  ) =>
  Eval AsImplMoneyConst b (IO T.Money) where
  eval _ _ = eval AsImplMoneyConst (Proxy :: Proxy a)

instance KnownSymbol val =>
 Eval AsImplMoneyConst (L.MoneyValImpl val) (IO T.Money) where
  eval _ _ = pure $ read $ symbolVal (Proxy :: Proxy val)     -- unsafe

-- Interpreting a LotPayload value

instance
  ( b ~ L.MkLotPayload a
  , Eval AsImplLotPayload a (IO StateContext)
  ) =>
  Eval AsImplLotPayload b (IO StateContext) where
  eval _ _ = eval AsImplLotPayload (Proxy :: Proxy a)

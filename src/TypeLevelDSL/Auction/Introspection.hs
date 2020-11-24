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

module TypeLevelDSL.Auction.Introspection
  ( module X
  , AsAuction (..)
  , describeAuction
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import TypeLevelDSL.Auction.Description.Language
import TypeLevelDSL.Auction.Flow.Language
import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Auction.Description.Introspection as X
import TypeLevelDSL.Auction.Flow.Introspection as X
import TypeLevelDSL.Eval

-- Interpretation tags

data AsAuction = AsAuction

-- Interpreting of the Auction


instance
  ( Eval AsInfo info [String]
  , Eval AsLots lots [String]
  , Eval AsAuctionFlow flow [String]
  ) =>
  Eval AsAuction (Auction' flow info lots) [String] where
  eval _ _ = do

    -- start the flow
    strs1 <- eval AsInfo (Proxy :: Proxy info)
    strs2 <- eval AsLots (Proxy :: Proxy lots)

    -- get a min bid for a lot
    strs3 <- eval AsAuctionFlow (Proxy :: Proxy flow)
    pure $ "==> Auction! <==" : (strs1 <> strs2 <> strs3)


describeAuction
  :: Eval AsAuction auction a
  => Proxy auction -> IO a
describeAuction p = eval AsAuction p

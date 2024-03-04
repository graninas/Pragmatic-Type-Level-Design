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

module Auction.Introspection.Auction
  ( module X
  , AsIntroAuction (..)
  , describeAuction
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Auction.Language.Description
import Auction.Language.Flow
import Auction.Language.Auction
import Auction.Introspection.Description as X
import Auction.Introspection.Flow as X
import TypeLevelDSL.Eval

-- Interpretation tags

data AsIntroAuction = AsIntroAuction

-- Interpreting of the Auction


instance
  ( Eval AsIntroInfo info (IO [String])
  , Eval AsIntroLots lots (IO [String])
  , Eval AsIntroAuctionFlow flow (IO [String])
  ) =>
  Eval AsIntroAuction (Auction' flow info lots) (IO [String]) where
  eval _ _ = do

    -- start the flow
    strs1 <- eval AsIntroInfo (Proxy :: Proxy info)
    strs2 <- eval AsIntroLots (Proxy :: Proxy lots)

    -- get a min bid for a lot
    strs3 <- eval AsIntroAuctionFlow (Proxy :: Proxy flow)
    pure $ "==> Auction! <==" : (strs1 <> strs2 <> strs3)


describeAuction
  :: Eval AsIntroAuction auction (IO a)
  => Proxy auction -> IO a
describeAuction p = eval AsIntroAuction p

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

module TypeLevelDSL.Auction.Introspection.Auction
  ( module X
  , AsIntroAuction (..)
  , describeAuction
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import TypeLevelDSL.Auction.Language.Description
import TypeLevelDSL.Auction.Language.Flow
import TypeLevelDSL.Auction.Language.Auction
import TypeLevelDSL.Auction.Introspection.Description as X
import TypeLevelDSL.Auction.Introspection.Flow as X
import TypeLevelDSL.Eval

-- Interpretation tags

data AsIntroAuction = AsIntroAuction

-- Interpreting of the Auction


instance
  ( Eval AsIntroInfo info [String]
  , Eval AsIntroLots lots [String]
  , Eval AsIntroAuctionFlow flow [String]
  ) =>
  Eval AsIntroAuction (Auction' flow info lots) [String] where
  eval _ _ = do

    -- start the flow
    strs1 <- eval AsIntroInfo (Proxy :: Proxy info)
    strs2 <- eval AsIntroLots (Proxy :: Proxy lots)

    -- get a min bid for a lot
    strs3 <- eval AsIntroAuctionFlow (Proxy :: Proxy flow)
    pure $ "==> Auction! <==" : (strs1 <> strs2 <> strs3)


describeAuction
  :: Eval AsIntroAuction auction a
  => Proxy auction -> IO a
describeAuction p = eval AsIntroAuction p

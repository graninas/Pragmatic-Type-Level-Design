{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.AuctionDSL where

import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Auction.Exts

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- data GetPayloadValueTag a
-- data FunctionTag a

-- type family MkSetRef (a :: *) :: ActionTag a
-- type family MkGetRef (a :: *) :: ActionTag a
--
-- -- type family MkGetPayloadValue (a :: *) :: GetPayloadValueTag a

--
--
-- data SetRef' (refName :: Symbol) (src :: FunctionTag a)
-- data GetRef' (refName :: Symbol) (src :: FunctionTag a)
--
-- data Action (act :: ActionTag a)
--
-- type SetRef refName src = Action (MkSetRef (SetRef' refName src))
-- type GetRef refName src = Action (MkGetRef (GetRef' refName src))


data AuctionFlowTag a
data LotProcessTag a
data ActionTag a
data LambdaTag a

type family MkAuctionFlow (a :: *) :: AuctionFlowTag a
type family MkLotProcess  (a :: *) :: LotProcessTag a
type family MkAction      (a :: *) :: ActionTag a
type family MkLambda      (a :: *) :: LambdaTag a

data AuctionFlow'     (lotProcess :: LotProcessTag lp)
data LotProcess'      (startActions :: *)
data GetPayloadValue' (valName :: Symbol) (cont :: LambdaTag lam)
data End'
data Print'
data Drop'

type AuctionFlow lotProcess       = MkAuctionFlow (AuctionFlow' lotProcess)
type LotProcess startActions      = MkLotProcess (LotProcess' startActions)
type GetPayloadValue valName cont = MkAction (GetPayloadValue' valName cont)
type End                          = MkAction End'
type Print                        = MkLambda Print'
type Drop                         = MkLambda Drop'

data Action (act :: ActionTag a) acts

-- Non-type-safe. The type of minBid is not checked somehow.
-- (This proves that when you lift to the type level, you have
--  to reestablish type safety, if you really need it).

-- You have to decide on how low level your DSL should be.
--   Should it be a domain-level DSL, or it should include
--   a limited but domain-agnostic programming language.


-- English Auction Flow

-- Greeting
-- Iterations
-- Bid
-- Lot
-- Notification
-- Final condition
-- Round

-- Lenses?
-- data LotPayloadGetter
-- data MinBidGetter
-- data Get l r

type EnglishAuctionFlow = AuctionFlow
  ( LotProcess
      ( Action (GetPayloadValue "minBid" Print)
        ( Action (GetPayloadValue "minBid" Drop)
          End
        )
      )
  )

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- type family Lots (a :: [*]) :: LotsTag a
--                                        ^
{-# LANGUAGE PolyKinds                #-}

-- data D (name :: Symbol) (ps :: [Country])
--                 ^              ^
{-# LANGUAGE KindSignatures           #-}

module TypeLevelDSL.Auction.Description.Language where

import GHC.TypeLits (Symbol)

-- Closed data

data Country
  = US
  | UK
  | UA
  deriving (Show, Read, Eq, Ord)


-- TODO: try make Symbol work instead of type marks
data MinBid

-- eDSL

data DynVal (name :: Symbol)

data MoneyVal' (val :: Symbol)

data Info' (name :: Symbol) (holder :: Symbol)

data Lot (name :: Symbol)
         (descr :: Symbol)
         (payload :: LotPayloadTag p)
         (currency :: CurrencyTag a)
         (censorship :: CensorshipTag c)

-- Extension points:

data MoneyConstTag a
data AuctionInfoTag a
data LotsTag a
data LotPayloadTag a
data CurrencyTag a
data CensorshipTag a
data BidTag a

-- Construction

type family MoneyConst (a :: *) :: MoneyConstTag a

type family AuctionInfo (a :: *) :: AuctionInfoTag a

type family Currency (a :: *) :: CurrencyTag a

type family Censorship (a :: *) :: CensorshipTag a

type family Lots (a :: [*]) :: LotsTag a

type family LotPayload (a :: *) :: LotPayloadTag a

type family Bid (a :: *) :: BidTag a

-- Helpers

data NoCensorship'    -- This can be pattern matched esier than something like ()
type NoCensorship = Censorship NoCensorship'

type Info name holder = AuctionInfo (Info' name holder)

type MoneyVal (val :: Symbol)     = MoneyConst (MoneyVal' val)
type MoneyDynVal (name :: Symbol) = MoneyConst (DynVal name)

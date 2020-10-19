{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- Variable ‘aType’ occurs more often
        -- in the constraint ‘Eval AsType aType String’
        -- than in the instance head ‘Eval AsInfo b ()’
{-# LANGUAGE UndecidableInstances     #-}

-- instance (Eval AsEngine engine (), Eval AsLots parts ()) =>
--          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleContexts         #-}

-- instance (b ~ Parts a, Eval AsLots a ()) => Eval AsLots b ()
--                                             ^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Auction.Implementation where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Eval


-- Interpretation tags

data AsInfo = AsInfo
data AsAuction = AsAuction
data AsAuctionType = AsAuctionType
data AsLots = AsLots
data AsLot = AsLot
data AsCensorship = AsCensorship
data AsCurrency = AsCurrency
data AsMoneyConst = AsMoneyConst


-- Interpreting of the Auction

type Ret = [String]

instance (Eval AsInfo info Ret, Eval AsLots lots Ret) =>
  Eval AsAuction (Auction info lots) Ret where
  eval _ _ = do
    strs1 <- eval AsInfo (Proxy :: Proxy info)
    strs2 <- eval AsLots (Proxy :: Proxy lots)
    pure $ "==> Auction! <==" : (strs1 <> strs2)


-- Interpreting of the auction info (auctionInfo :: AuctionInfoTag)

instance (ai ~ AuctionInfo i, Eval AsInfo i Ret) =>
  Eval AsInfo ai Ret where
  eval _ _ = eval AsInfo (Proxy :: Proxy i)

instance (Eval AsAuctionType aType String, KnownSymbol name, KnownSymbol holder) =>
  Eval AsInfo (Info' name aType holder) Ret where
  eval _ _ = do
    typeName <- eval AsAuctionType (Proxy :: Proxy aType)
    pure $ ( "Name: " <> symbolVal (Proxy :: Proxy name) )
         : ( "Holder: " <> symbolVal (Proxy :: Proxy holder) )
         : ( "Type: " <> typeName )
         : []

-- Interpreting of the AuctionType

instance Eval AsAuctionType EnglishAuction String where
  eval _ _ = pure "EnglishAuction"


-- Interpreting of the list of lots (lots :: LotsTag a)

-- No instance for an empty list. Empty lists are prohibited.
-- instance Eval AsLots '[] Ret where
--   eval _ _ = pure []

-- N.B., item is interpreted AsLot
instance Eval AsLot p Ret => Eval AsLots (p ': '[]) Ret where
  eval _ _ = eval AsLot (Proxy :: Proxy p)

-- N.B., item is interpreted AsLot
instance (Eval AsLot p Ret, Eval AsLots (x ': ps) Ret) =>
  Eval AsLots (p ': x ': ps) Ret where
  eval _ _ = do
    strs1 <- eval AsLot (Proxy :: Proxy p)
    strs2 <- eval AsLots (Proxy :: Proxy (x ': ps))
    pure $ strs1 <> strs2

instance (b ~ Lots a, Eval AsLots a Ret) => Eval AsLots b Ret where
  eval _ _ = eval AsLots (Proxy :: Proxy a)


-- Interpreting of a Lot

instance (Eval AsCurrency currency Ret, Eval AsCensorship censorship Ret,
  Eval AsMoneyConst minBid String,
  KnownSymbol name, KnownSymbol descr) =>
  Eval AsLot (Lot name descr minBid currency censorship) Ret where
  eval _ _ = do
    minBid <- eval AsMoneyConst (Proxy :: Proxy minBid)
    censorship <- eval AsCensorship (Proxy :: Proxy censorship)
    currency   <- eval AsCurrency (Proxy :: Proxy currency)
    pure $ ( "Lot: " <> symbolVal (Proxy :: Proxy name) )
         : ( "Description: " <> symbolVal (Proxy :: Proxy descr) )
         : ( "Minimum bid: " <> minBid )
         : ( currency <> censorship )


-- Interpreting of the Currency extension

instance (b ~ Currency a, Eval AsCurrency a Ret) =>
  Eval AsCurrency b Ret where
  eval _ _ = eval AsCurrency (Proxy :: Proxy a)


-- Interpreting of the Censorship extension

instance (b ~ Censorship a, Eval AsCensorship a Ret) =>
  Eval AsCensorship b Ret where
  eval _ _ = eval AsCensorship (Proxy :: Proxy a)


-- Interpretating of the NoCensorship

instance Eval AsCensorship NoCensorship' Ret where
  eval _ _ = pure []


-- Interpreting a MoneyConst value
instance (b ~ MoneyConst a, Eval AsMoneyConst a String) =>
  Eval AsMoneyConst b String where
  eval _ _ = eval AsMoneyConst (Proxy :: Proxy a)


instance KnownSymbol val => Eval AsMoneyConst (MoneyVal' val) String where
  eval _ _ = pure $ symbolVal (Proxy :: Proxy val)

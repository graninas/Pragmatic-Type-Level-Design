{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- Variable ‘aType’ occurs more often
        -- in the constraint ‘Eval AsIntroType aType String’
        -- than in the instance head ‘Eval AsIntroInfo b ()’
{-# LANGUAGE UndecidableInstances     #-}

-- instance (Eval AsIntroEngine engine (), Eval AsIntroLots parts ()) =>
--          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleContexts         #-}

-- instance (b ~ Parts a, Eval AsIntroLots a ()) => Eval AsIntroLots b ()
--                                             ^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleInstances        #-}

module Auction.Introspection.Description where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Auction.Language.Description
import TypeLevelDSL.Eval


-- Interpretation tags

data AsIntroInfo = AsIntroInfo
data AsIntroLots = AsIntroLots
data AsIntroLot = AsIntroLot
data AsIntroCensorship = AsIntroCensorship
data AsIntroCurrency = AsIntroCurrency
data AsIntroMoneyConst = AsIntroMoneyConst
data AsIntroLotPayload = AsIntroLotPayload
data AsIntroBid = AsIntroBid
data AsIntroMinBid = AsIntroMinBid


-- Interpreting of the auction info (auctionInfo :: IAuctionInfo)

instance
  ( ai ~ MkAuctionInfo i
  , Eval AsIntroInfo i (IO [String])
  ) =>
  Eval AsIntroInfo ai (IO [String]) where
  eval _ _ = eval AsIntroInfo (Proxy :: Proxy i)

instance
  ( KnownSymbol name
  , KnownSymbol holder
  ) =>
  Eval AsIntroInfo (Info' name holder) (IO [String]) where
  eval _ _ = do
    pure $ ( "Name: " <> symbolVal (Proxy :: Proxy name) )
         : ( "Holder: " <> symbolVal (Proxy :: Proxy holder) )
         : []

-- Interpreting of the list of lots (lots :: ILots)

-- No instance for an empty list. Empty lists are prohibited.
-- instance Eval AsIntroLots '[] [String] where
--   eval _ _ = pure []

-- N.B., item is interpreted AsIntroLot
instance Eval AsIntroLot p (IO [String]) =>
  Eval AsIntroLots (p ': '[]) (IO [String]) where
  eval _ _ = eval AsIntroLot (Proxy :: Proxy p)

-- N.B., item is interpreted AsIntroLot
instance
  ( Eval AsIntroLot p (IO [String])
  , Eval AsIntroLots (x ': ps) (IO [String])
  ) =>
  Eval AsIntroLots (p ': x ': ps) (IO [String]) where
  eval _ _ = do
    strs1 <- eval AsIntroLot (Proxy :: Proxy p)
    strs2 <- eval AsIntroLots (Proxy :: Proxy (x ': ps))
    pure $ strs1 <> strs2

instance
  ( b ~ MkLots a
  , Eval AsIntroLots a (IO [String])
  ) =>
  Eval AsIntroLots b (IO [String]) where
  eval _ _ = eval AsIntroLots (Proxy :: Proxy a)


-- Interpreting of a Lot

instance
  ( Eval AsIntroCurrency currency (IO [String])
  , Eval AsIntroCensorship censorship (IO [String])
  , Eval AsIntroLotPayload payload (IO String)
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval AsIntroLot (Lot' name descr payload currency censorship) (IO [String]) where
  eval _ _ = do
    payload    <- eval AsIntroLotPayload (Proxy :: Proxy payload)
    censorship <- eval AsIntroCensorship (Proxy :: Proxy censorship)
    currency   <- eval AsIntroCurrency (Proxy :: Proxy currency)
    pure $ ( "Lot: " <> symbolVal (Proxy :: Proxy name) )
         : ( "Description: " <> symbolVal (Proxy :: Proxy descr) )
         :   payload
         : ( currency <> censorship )


-- Interpreting of the Currency extension

instance
  ( b ~ MkCurrency a
  , Eval AsIntroCurrency a (IO [String])
  ) =>
  Eval AsIntroCurrency b (IO [String]) where
  eval _ _ = eval AsIntroCurrency (Proxy :: Proxy a)


-- Interpreting of the Censorship extension

instance
  ( b ~ MkCensorship a
  , Eval AsIntroCensorship a (IO [String])
  ) =>
  Eval AsIntroCensorship b (IO [String]) where
  eval _ _ = eval AsIntroCensorship (Proxy :: Proxy a)


-- Interpretating of the NoCensorship

instance Eval AsIntroCensorship NoCensorship' (IO [String]) where
  eval _ _ = pure []


-- Interpreting a MoneyConst value

instance
  ( b ~ MkMoneyConst a
  , Eval AsIntroMoneyConst a (IO String)
  ) =>
  Eval AsIntroMoneyConst b (IO String) where
  eval _ _ = eval AsIntroMoneyConst (Proxy :: Proxy a)

instance KnownSymbol val =>
  Eval AsIntroMoneyConst (MoneyVal' val) (IO String) where
  eval _ _ = pure $ symbolVal (Proxy :: Proxy val)

-- Interpreting a LotPayload value

instance
  ( b ~ MkLotPayload a
  , Eval AsIntroLotPayload a (IO String)
  ) =>
  Eval AsIntroLotPayload b (IO String) where
  eval _ _ = eval AsIntroLotPayload (Proxy :: Proxy a)

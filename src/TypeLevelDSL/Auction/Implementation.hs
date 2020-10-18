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
data AsType = AsType
data AsLots = AsLots
data AsLot = AsLot
data AsCensorship = AsCensorship
data AsCurrency = AsCurrency


-- Interpreting of the Auction

instance (Eval AsInfo info (), Eval AsLots lots ()) =>
  Eval AsAuction (Auction info lots) () where
  eval _ _ = do
    putStrLn "This is an auction."
    eval AsInfo (Proxy :: Proxy info)
    eval AsLots (Proxy :: Proxy lots)


-- Interpreting of the auction info (auctionInfo :: AuctionInfoTag)

instance (b ~ Info name aType holder, Eval AsType aType String,
          KnownSymbol name, KnownSymbol holder) =>
  Eval AsInfo b () where
  eval _ _ = do
    typeName <- eval AsType (Proxy :: Proxy aType)
    putStrLn $ "Name: " <> symbolVal (Proxy :: Proxy name)
    putStrLn $ "Holder: " <> symbolVal (Proxy :: Proxy holder)
    putStrLn $ "Type: " <> typeName

-- Interpreting of the AuctionType

instance Eval AsType EnglishAuction String where
  eval _ _ = pure "EnglishAuction"


-- Interpreting of the list of lots (lots :: LotsTag a)

-- No instance for an empty list. Empty lists are prohibited.
-- instance Eval AsLots '[] () where
--   eval _ _ = pure ()

-- N.B., item is interpreted AsLot
instance Eval AsLot p () => Eval AsLots (p ': '[]) () where
  eval _ _ = eval AsLot (Proxy :: Proxy p)

-- N.B., item is interpreted AsLot
instance (Eval AsLot p (), Eval AsLots (x ': ps) ()) =>
  Eval AsLots (p ': x ': ps) () where
  eval _ _ = do
    eval AsLot (Proxy :: Proxy p)
    eval AsLots (Proxy :: Proxy (x ': ps))

instance (b ~ Lots a, Eval AsLots a ()) => Eval AsLots b () where
  eval _ _ = eval AsLots (Proxy :: Proxy a)


-- Interpreting of a Lot

instance (Eval AsCurrency currency (), Eval AsCensorship censorship (),
  KnownSymbol name, KnownSymbol descr) =>
  Eval AsLot (Lot name descr currency censorship) () where
  eval _ _ = do
    censorship <- eval AsCensorship (Proxy :: Proxy censorship)
    currency   <- eval AsCurrency (Proxy :: Proxy currency)
    putStrLn $ "Lot: " <> symbolVal (Proxy :: Proxy name)
    putStrLn $ "Description: " <> symbolVal (Proxy :: Proxy descr)

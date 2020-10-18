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

-- instance (Eval AsEngine engine (), Eval AsPart parts ()) =>
--          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleContexts         #-}

-- instance (b ~ Parts a, Eval AsPart a ()) => Eval AsPart b ()
--                                             ^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Auction.Implementation where

import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Eval

-- Interpreting of the Auction

data AsAuction = AsAuction

instance (Eval AsInfo info (), Eval AsLot lots ()) =>
  Eval AsAuction (Auction info lots) () where
  eval _ _ = do
    putStrLn "This is an auction."
    eval AsInfo (Proxy :: Proxy info)
    eval AsLot (Proxy :: Proxy lots)


-- Interpreting of the (auctionInfo :: AuctionInfoTag)

data AsInfo = AsInfo

instance (b ~ Info name aType holder, Eval AsType aType String,
          KnownSymbol name, KnownSymbol holder) =>
  Eval AsInfo b () where
  eval _ _ = do
    putStrLn $ "Name: " <> symbolVal (Proxy :: Proxy name)
    putStrLn $ "Holder: " <> symbolVal (Proxy :: Proxy holder)
    typeName <- eval AsType (Proxy :: Proxy aType)
    putStrLn $ "Type: " <> typeName

-- Interpreting of the AuctionType

data AsType = AsType

instance Eval AsType EnglishAuction String where
  eval _ _ = pure "EnglishAuction"

-- Interpreting of the (lots :: LotsTag a)

data AsLot = AsLot

--
-- instance Eval AsPart '[] () where
--   eval _ _ = pure ()
--
-- instance Eval AsPart p () => Eval AsPart (p ': '[]) () where
--   eval _ _ = eval AsPart (Proxy :: Proxy p)
--
-- instance (Eval AsPart p (), Eval AsPart (x ': ps) ()) => Eval AsPart (p ': x ': ps) () where
--   eval _ _ = do
--     eval AsPart (Proxy :: Proxy p)
--     eval AsPart (Proxy :: Proxy (x ': ps))
--
-- instance (b ~ Parts a, Eval AsPart a ()) => Eval AsPart b () where
--   eval _ _ = eval AsPart (Proxy :: Proxy a)

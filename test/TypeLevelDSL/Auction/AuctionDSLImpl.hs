{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module TypeLevelDSL.Auction.AuctionDSLImpl where

import TypeLevelDSL.Auction.Language
import TypeLevelDSL.Auction.AuctionDSL
import TypeLevelDSL.Eval

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

-- Implementation

-- Interpreting of the participants list

data AsAuctioinDSL = AsAuctioinDSL
data AsLotProcess  = AsLotProcess
data AsAction      = AsAction

-- Lot Process

instance (EvalCtx ctx AsAction acts ()) =>
  EvalCtx ctx AsLotProcess (LotProcess' acts) () where
  evalCtx ctx _ _ = do
    putStrLn "Lot process"
    evalCtx ctx AsAction (Proxy :: Proxy acts)

instance (mkProc ~ MkLotProcess proc, EvalCtx ctx AsLotProcess proc ()) =>
  EvalCtx ctx AsLotProcess mkProc () where
  evalCtx ctx _ _ = evalCtx ctx AsLotProcess (Proxy :: Proxy proc)

-- The Actions mechanism

-- This is how we unwrap a type constructed with a type family.
-- Example:
-- type End = MkAction End'
--      ^ type to unwrap
--            ^ type family
--                     ^ some real data type

instance (mkAct ~ MkAction act, EvalCtx ctx AsAction act ()) =>
  EvalCtx ctx AsAction mkAct () where
  evalCtx ctx _ _ = evalCtx ctx AsAction (Proxy :: Proxy act)

instance EvalCtx ctx AsAction End' () where
  evalCtx ctx _ _ = putStrLn "DEBUG: End reached."

instance (EvalCtx ctx AsAction act (), EvalCtx ctx AsAction acts ()) =>
  EvalCtx ctx AsAction (Action' act acts) () where
  evalCtx ctx _ _ = do
    evalCtx ctx AsAction (Proxy :: Proxy act)
    evalCtx ctx AsAction (Proxy :: Proxy acts)

-- Specific actions

instance (Show valType, HasValue ctx valName valType) =>
  EvalCtx ctx AsAction (GetPayloadValue' valName valType lam) () where
  evalCtx ctx _ _ = do
    putStrLn $ show $ ((getVal @_ @valName @valType) ctx)
    putStrLn "GetPayloadValue' reached"

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

module TypeLevelDSL.Auction.Implementation.DataActions where

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import qualified TypeLevelDSL.Auction.Implementation.Action as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context

import qualified Data.Dynamic as Dyn
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- Specific actions

-- instance Eval Impl.AsImplAction (L.GetPayloadValue' valName valType lam) [String] where
  -- eval _ _ = pure ["GetPayloadValue' reached"]

instance
  (Context ctx, KnownSymbol refName, Dyn.Typeable refType)
  => EvalCtx ctx Impl.AsImplAction (L.ReadRef' refName refType lam) [String] where
  evalCtx ctx _ _ = do
    putStrLn "ReadRef' reached"

    let valName = (symbolVal (Proxy :: Proxy refName))
    mbDyn <- getDyn ctx valName
    mbDyn' <- getDyn' @ctx @refType ctx valName
    case mbDyn of
      Nothing -> error $ "Value " <> valName <> " not found."
      Just dyn -> case Dyn.fromDynamic dyn of
        Nothing -> error "no conversion"
        Just (v :: refType) -> putStrLn $ "Successfully fetched value " <> valName <> "."
    case mbDyn' of
      Nothing -> error $ "Value " <> valName <> " not found."
      Just dyn -> case Dyn.fromDynamic dyn of
        Nothing -> error "no conversion"
        Just (v :: refType) -> putStrLn $ "Successfully fetched value " <> valName <> "."
    pure ["ReadRef' reached"]

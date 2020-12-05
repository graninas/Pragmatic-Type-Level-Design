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
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  , EvalLambdaCtx ctx refType Impl.AsImplLambda lam [String]
  )
  => EvalCtx ctx Impl.AsImplAction (L.ReadRef' refName refType lam) [String] where
  evalCtx ctx _ _ = do
    let valName = (symbolVal (Proxy :: Proxy refName))
    mbDyn <- getDyn ctx valName
    let mbVal = mbDyn >>= Dyn.fromDynamic
    case mbVal of
      Nothing -> error $ "Value " <> valName <> " not found."
      Just (val :: refType) -> do
        putStrLn $ "Successfully fetched value " <> valName <> "."
        evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []


-- Specific lambdas

instance Show val
  => EvalLambdaCtx ctx val Impl.AsImplLambda L.Print' [String] where
  evalLambdaCtx _ val _ _ = pure [show val]

instance Show val
  => EvalLambdaCtx ctx val Impl.AsImplLambda L.Drop' [String] where
  evalLambdaCtx _ _ _ _ = pure []

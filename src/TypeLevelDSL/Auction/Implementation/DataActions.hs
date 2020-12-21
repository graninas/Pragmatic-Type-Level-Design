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

import qualified TypeLevelDSL.Auction.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Action as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context

import Data.Typeable (Typeable)
import qualified Data.Dynamic as Dyn
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- * Specific actions

-- GetPayloadValue

instance
  ( Context ctx
  , Typeable valTag
  , Dyn.Typeable valType
  , EvalLambdaCtx ctx valType Impl.AsImplLambda lam [String]
  ) =>
  EvalCtx ctx Impl.AsImplAction (L.GetPayloadValue' valTag valType lam) [String] where
  evalCtx ctx _ _ = do
    let key = toTypeableKey @valTag
    withContextValue ctx key
      $ \(val :: valType) -> evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []


-- ReadRef

instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  , EvalLambdaCtx ctx refType Impl.AsImplLambda lam [String]
  )
  => EvalCtx ctx Impl.AsImplAction (L.ReadRef' refName refType lam) [String] where
  evalCtx ctx _ _ = do
    let valName = symbolVal (Proxy :: Proxy refName)
    withContextValue ctx valName
      $ \(val :: refType) -> evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []


-- * Specific lambdas

-- Print lambda
instance Show val
  => EvalLambdaCtx ctx val Impl.AsImplLambda L.Print' [String] where
  evalLambdaCtx _ val _ _ = pure [show val]

-- Drop lambda
instance EvalLambdaCtx ctx val Impl.AsImplLambda L.Drop' [String] where
  evalLambdaCtx _ _ _ _ = pure []

-- WriteRef lambda
instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  )
  => EvalLambdaCtx ctx refType Impl.AsImplLambda (L.WriteRef' refName refType) [String] where
  evalLambdaCtx ctx val _ _ = do
    let valName = symbolVal (Proxy :: Proxy refName)
    let dynVal = Dyn.toDyn val
    setDyn ctx valName dynVal (Proxy :: Proxy refType)
    pure []


withContextValue
  :: forall refType ctx a
   . Context ctx
  => Dyn.Typeable refType
  => ctx
  -> String
  -> (refType -> IO a)
  -> IO a
withContextValue ctx valName f = do
    mbDyn <- getDyn ctx valName (Proxy :: Proxy refType)
    case mbDyn of
      Nothing -> error $ "Value " <> valName <> " not found."
      Just dyn -> case Dyn.fromDynamic dyn of
        Nothing -> error $ "Value " <> valName <> " not parsed."
        Just (val :: refType) -> do
          putStrLn $ "Successfully fetched value " <> valName <> "."
          f val

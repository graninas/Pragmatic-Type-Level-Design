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

module TypeLevelDSL.Implementation.DataActions where

import qualified TypeLevelDSL.Language as L
import qualified TypeLevelDSL.Implementation.Action as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context

import Data.Typeable (Typeable)
import qualified Data.Dynamic as Dyn
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


-- ReadRef

instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  , EvalLambdaCtx ctx refType Impl.AsImplLambda lam (IO [String])
  )
  => EvalCtx ctx Impl.AsImplAction (L.ReadRefImpl refName refType lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let valName = symbolVal (Proxy :: Proxy refName)
    withContextValue ctx valName
      $ \(val :: refType) -> evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []


-- Both lambda

instance
  ( Context ctx
  , EvalLambdaCtx ctx val Impl.AsImplLambda lam1 (IO [String])
  , EvalLambdaCtx ctx val Impl.AsImplLambda lam2 (IO [String])
  )
  => EvalLambdaCtx ctx val Impl.AsImplLambda (L.BothImpl lam1 lam2) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam1)
    evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam2)
    pure []


-- Print lambda
instance Show val
  => EvalLambdaCtx ctx val Impl.AsImplLambda L.PrintImpl (IO [String]) where
  evalLambdaCtx _ val _ _ = pure [show val]

-- Drop lambda
instance EvalLambdaCtx ctx val Impl.AsImplLambda L.DropImpl (IO [String]) where
  evalLambdaCtx _ _ _ _ = pure []

-- ConcatL lambda
instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambdaCtx ctx String Impl.AsImplLambda lam (IO [String])
  )
  => EvalLambdaCtx ctx String Impl.AsImplLambda (L.ConcatLImpl str lam) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    let lStr = symbolVal (Proxy :: Proxy str)
    evalLambdaCtx ctx (lStr ++ val) Impl.AsImplLambda (Proxy :: Proxy lam)

-- ConcatR lambda
instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambdaCtx ctx String Impl.AsImplLambda lam (IO [String])
  )
  => EvalLambdaCtx ctx String Impl.AsImplLambda (L.ConcatRImpl lam str) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    let rStr = symbolVal (Proxy :: Proxy str)
    evalLambdaCtx ctx (val ++ rStr) Impl.AsImplLambda (Proxy :: Proxy lam)

-- WriteRef lambda
instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  )
  => EvalLambdaCtx ctx refType Impl.AsImplLambda (L.WriteRefImpl refName refType) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    let valName = symbolVal (Proxy :: Proxy refName)
    let dynVal = Dyn.toDyn val
    setDyn ctx valName dynVal
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
    mbDyn <- getDyn ctx valName
    case mbDyn of
      Nothing -> error $ "Value " <> valName <> " not found."
      Just dyn -> case Dyn.fromDynamic dyn of
        Nothing -> error $ "Value " <> valName <> " not parsed."
        Just (val :: refType) -> do
          putStrLn $ "Successfully fetched value " <> valName <> "."
          f val

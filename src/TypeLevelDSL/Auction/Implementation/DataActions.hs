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
{-# LANGUAGE UndecidableInstances     #-}

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

-- TODO: rework get payload value
-- instance Eval Impl.AsImplAction (L.GetPayloadValue' valName valType lam) [String] where
  -- eval _ _ = pure ["GetPayloadValue' reached"]

data AsRefTag = AsRefTag

instance
  ( KnownSymbol refName
  )
  => Eval AsRefTag (L.RefTag' refName refType) (String, Proxy refType) where
  eval _ _ = do
    let refName = symbolVal (Proxy :: Proxy refName)
    let proxy = proxy :: Proxy refType
    pure (refName, proxy)


instance
  ( mkRef ~ L.MkRefTag ref
  , Eval AsRefTag ref (String, Proxy refType)
  )
  => Eval AsRefTag mkRef (String, Proxy refType) where
  eval _ _ = eval AsRefTag (Proxy :: Proxy ref)




instance
  ( Context ctx
  , Dyn.Typeable refType
  -- , EvalLambdaCtx ctx refType Impl.AsImplLambda lam [String]
  )
  => EvalCtx ctx Impl.AsImplAction (L.ReadRef' refTag lam) [String] where
  evalCtx ctx _ _ = do
    -- (refName, proxy :: Proxy refType) <- eval AsRefTag (Proxy :: Proxy refTag)
    (refName, proxy) <- eval AsRefTag (Proxy :: Proxy refTag)

    mbDyn <- getDyn ctx refName proxy

    case mbDyn >>= Dyn.fromDynamic of
      Nothing -> error $ "Reference " <> refName <> " not found."
      Just (val :: refType) -> do
        putStrLn $ "Successfully fetched reference " <> refName <> "."
        evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []


-- Specific lambdas

instance Show val
  => EvalLambdaCtx ctx val Impl.AsImplLambda L.Print' [String] where
  evalLambdaCtx _ val _ _ = pure [show val]

instance Show val
  => EvalLambdaCtx ctx val Impl.AsImplLambda L.Drop' [String] where
  evalLambdaCtx _ _ _ _ = pure []

instance
  ( Context ctx
  -- , KnownSymbol refName
  , Dyn.Typeable refType
  )
  => EvalLambdaCtx ctx refType Impl.AsImplLambda (L.WriteRef' refTag) [String] where
  evalLambdaCtx ctx val _ _ = do
    (refName, proxy) <- eval AsRefTag (Proxy :: Proxy refTag)

    -- let refName = symbolVal (Proxy :: Proxy refName)

    let dynVal = Dyn.toDyn val
    setDyn ctx refName dynVal (Proxy :: Proxy refType)
    pure []

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

module Auction.Implementation.DataActions where

import qualified TypeLevelDSL.Language as L
import qualified Auction.Language as L
import qualified TypeLevelDSL.Implementation.Action as Impl
import qualified TypeLevelDSL.Implementation.DataActions as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Dyn as Dyn

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
  , EvalLambdaCtx ctx valType Impl.AsImplLambda lam (IO [String])
  ) =>
  EvalCtx ctx Impl.AsImplAction (L.GetPayloadValue' valTag valType lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let key = Dyn.toTypeableKey @valTag
    Impl.withContextValue ctx key
      $ \(val :: valType) -> evalLambdaCtx ctx val Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []

-- GetLotName

instance
  ( Context ctx
  , EvalLambdaCtx ctx String Impl.AsImplLambda lam (IO [String])
  ) =>
  EvalCtx ctx Impl.AsImplAction (L.GetLotName' lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let key = "LotName"         -- FIXME: magic constant
    Impl.withContextValue ctx key
      $ \(lotName :: String) -> evalLambdaCtx ctx lotName Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []

-- GetLotDescr

instance
  ( Context ctx
  , EvalLambdaCtx ctx String Impl.AsImplLambda lam (IO [String])
  ) =>
  EvalCtx ctx Impl.AsImplAction (L.GetLotDescr' lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let key = "LotDescr"         -- FIXME: magic constant
    Impl.withContextValue ctx key
      $ \(lotDescr :: String) -> evalLambdaCtx ctx lotDescr Impl.AsImplLambda (Proxy :: Proxy lam)
    pure []

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

import Auction.Language
import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import qualified TypeLevelDSL.Dyn as Dyn

import Data.HList.HList
import Data.Typeable (Typeable)
import qualified Data.Dynamic as Dyn
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsImplAction = AsImplAction
data AsImplLambda = AsImplLambda

instance
  ( mkAct ~ MkHList act
  , EvalCtx ctx AsImplAction act (IO [String])
  ) =>
  EvalCtx ctx AsImplAction mkAct (IO [String]) where
  evalCtx ctx _ _ = evalCtx ctx AsImplAction (Proxy :: Proxy act)

instance EvalCtx ctx AsImplAction HEmptyImpl (IO [String]) where
  evalCtx ctx _ _ = pure ["HEmptyImpl reached."]

instance
  ( EvalCtx ctx AsImplAction act (IO [String])
  , EvalCtx ctx AsImplAction acts (IO [String])
  ) =>
  EvalCtx ctx AsImplAction (HListImpl act acts) (IO [String]) where
  evalCtx ctx _ _ = do
    strs1 <- evalCtx ctx AsImplAction (Proxy :: Proxy act)
    strs2 <- evalCtx ctx AsImplAction (Proxy :: Proxy acts)
    pure $ strs1 <> strs2

-- Lambda mechanism

instance
  ( mkLam ~ MkLambda lam
  , EvalLambdaCtx ctx valType AsImplLambda lam (IO [String])
  ) =>
  EvalLambdaCtx ctx valType AsImplLambda mkLam (IO [String]) where
  evalLambdaCtx ctx val _ _ =
    evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam)



-- * Specific actions

-- GetPayloadValue

instance
  ( Context ctx
  , Typeable valTag
  , Dyn.Typeable valType
  , EvalLambdaCtx ctx valType AsImplLambda lam (IO [String])
  ) =>
  EvalCtx ctx AsImplAction (GetPayloadValueImpl valTag valType lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let key = Dyn.toTypeableKey @valTag
    withContextValue ctx key
      $ \(val :: valType) -> evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam)
    pure []

-- GetLotName

instance
  ( Context ctx
  , EvalLambdaCtx ctx String AsImplLambda lam (IO [String])
  ) =>
  EvalCtx ctx AsImplAction (GetLotNameImpl lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let key = "LotName"         -- FIXME: magic constant
    withContextValue ctx key
      $ \(lotName :: String) -> evalLambdaCtx ctx lotName AsImplLambda (Proxy :: Proxy lam)
    pure []

-- GetLotDescr

instance
  ( Context ctx
  , EvalLambdaCtx ctx String AsImplLambda lam (IO [String])
  ) =>
  EvalCtx ctx AsImplAction (GetLotDescrImpl lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let key = "LotDescr"         -- FIXME: magic constant
    withContextValue ctx key
      $ \(lotDescr :: String) -> evalLambdaCtx ctx lotDescr AsImplLambda (Proxy :: Proxy lam)
    pure []



-- ReadRef

instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  , EvalLambdaCtx ctx refType AsImplLambda lam (IO [String])
  )
  => EvalCtx ctx AsImplAction (ReadRefImpl refName refType lam) (IO [String]) where
  evalCtx ctx _ _ = do
    let valName = symbolVal (Proxy :: Proxy refName)
    withContextValue ctx valName
      $ \(val :: refType) -> evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam)
    pure []


-- Both lambda

instance
  ( Context ctx
  , EvalLambdaCtx ctx val AsImplLambda lam1 (IO [String])
  , EvalLambdaCtx ctx val AsImplLambda lam2 (IO [String])
  )
  => EvalLambdaCtx ctx val AsImplLambda (BothImpl lam1 lam2) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam1)
    evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam2)
    pure []


-- Print lambda
instance Show val
  => EvalLambdaCtx ctx val AsImplLambda PrintImpl (IO [String]) where
  evalLambdaCtx _ val _ _ = pure [show val]

-- Drop lambda
instance EvalLambdaCtx ctx val AsImplLambda DropImpl (IO [String]) where
  evalLambdaCtx _ _ _ _ = pure []

-- ConcatL lambda
instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambdaCtx ctx String AsImplLambda lam (IO [String])
  )
  => EvalLambdaCtx ctx String AsImplLambda (ConcatLImpl str lam) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    let lStr = symbolVal (Proxy :: Proxy str)
    evalLambdaCtx ctx (lStr ++ val) AsImplLambda (Proxy :: Proxy lam)

-- ConcatR lambda
instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambdaCtx ctx String AsImplLambda lam (IO [String])
  )
  => EvalLambdaCtx ctx String AsImplLambda (ConcatRImpl lam str) (IO [String]) where
  evalLambdaCtx ctx val _ _ = do
    let rStr = symbolVal (Proxy :: Proxy str)
    evalLambdaCtx ctx (val ++ rStr) AsImplLambda (Proxy :: Proxy lam)

-- WriteRef lambda
instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  )
  => EvalLambdaCtx ctx refType AsImplLambda (WriteRefImpl refName refType) (IO [String]) where
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

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
  Eval ctx AsImplAction '[] (IO [String]) where
  eval ctx _ _ = pure []

instance
  ( Eval ctx AsImplAction act (IO [String])
  , Eval ctx AsImplAction acts (IO [String])
  ) =>
  Eval ctx AsImplAction (act ': acts) (IO [String]) where
  eval ctx _ _ = do
    strs1 <- eval ctx AsImplAction $ Proxy @act
    strs2 <- eval ctx AsImplAction $ Proxy @acts
    pure $ strs1 <> strs2

instance
  ( Eval ctx AsImplAction act (IO [String])
  ) =>
  Eval ctx AsImplAction
      ('ActionWrapper act)
      (IO [String]) where
  eval ctx _ _ =
    eval ctx AsImplAction $ Proxy @act

-- Lambda mechanism

instance
  ( mkLam ~ MkLambda lam
  , EvalLambda ctx valType AsImplLambda lam (IO [String])
  ) =>
  EvalLambda ctx valType AsImplLambda mkLam (IO [String]) where
  evalLambda ctx val _ _ =
    evalLambda ctx val AsImplLambda $ Proxy @lam

-- Specific actions

-- -- GetPayloadValue

instance
  ( Context ctx
  , Typeable valTag
  , Dyn.Typeable valType
  , EvalLambda ctx valType AsImplLambda lam (IO [String])
  ) =>
  Eval ctx AsImplAction (GetPayloadValueImpl valTag valType lam) (IO [String]) where
  eval ctx _ _ = do
    let key = Dyn.toTypeableKey @valTag
    withContextValue ctx key
      $ \(val :: valType) -> evalLambda ctx val AsImplLambda (Proxy :: Proxy lam)
    pure []

-- GetLotName

instance
  ( Context ctx
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  ) =>
  Eval ctx AsImplAction (GetLotNameImpl lam) (IO [String]) where
  eval ctx _ _ = do
    let key = "LotName"         -- FIXME: magic constant
    withContextValue ctx key
      $ \(lotName :: String) -> evalLambda ctx lotName AsImplLambda (Proxy :: Proxy lam)
    pure []

-- GetLotDescr

instance
  ( Context ctx
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  ) =>
  Eval ctx AsImplAction (GetLotDescrImpl lam) (IO [String]) where
  eval ctx _ _ = do
    let key = "LotDescr"         -- FIXME: magic constant
    withContextValue ctx key
      $ \(lotDescr :: String) -> evalLambda ctx lotDescr AsImplLambda (Proxy :: Proxy lam)
    pure []

-- ReadRef

instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  , EvalLambda ctx refType AsImplLambda lam (IO [String])
  )
  => Eval ctx AsImplAction (ReadRefImpl refName refType lam) (IO [String]) where
  eval ctx _ _ = do
    let valName = symbolVal (Proxy :: Proxy refName)
    withContextValue ctx valName
      $ \(val :: refType) -> evalLambda ctx val AsImplLambda (Proxy :: Proxy lam)
    pure []

-- Both lambda

instance
  ( Context ctx
  , EvalLambda ctx val AsImplLambda lam1 (IO [String])
  , EvalLambda ctx val AsImplLambda lam2 (IO [String])
  )
  => EvalLambda ctx val AsImplLambda (BothImpl lam1 lam2) (IO [String]) where
  evalLambda ctx val _ _ = do
    evalLambda ctx val AsImplLambda (Proxy :: Proxy lam1)
    evalLambda ctx val AsImplLambda (Proxy :: Proxy lam2)
    pure []


-- Print lambda
instance Show val
  => EvalLambda ctx val AsImplLambda PrintFImpl (IO [String]) where
  evalLambda _ val _ _ = pure [show val]

-- Drop lambda
instance EvalLambda ctx val AsImplLambda DropImpl (IO [String]) where
  evalLambda _ _ _ _ = pure []

-- ConcatL lambda
instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  )
  => EvalLambda ctx String AsImplLambda (ConcatLImpl str lam) (IO [String]) where
  evalLambda ctx val _ _ = do
    let lStr = symbolVal (Proxy :: Proxy str)
    evalLambda ctx (lStr ++ val) AsImplLambda (Proxy :: Proxy lam)

-- ConcatR lambda
instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  )
  => EvalLambda ctx String AsImplLambda (ConcatRImpl lam str) (IO [String]) where
  evalLambda ctx val _ _ = do
    let rStr = symbolVal (Proxy :: Proxy str)
    evalLambda ctx (val ++ rStr) AsImplLambda (Proxy :: Proxy lam)

-- WriteRef lambda
instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable refType
  )
  => EvalLambda ctx refType AsImplLambda (WriteRefImpl refName refType) (IO [String]) where
  evalLambda ctx val _ _ = do
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

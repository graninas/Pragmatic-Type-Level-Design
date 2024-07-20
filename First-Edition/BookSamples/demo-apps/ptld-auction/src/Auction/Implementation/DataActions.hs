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

instance
  ( EvalLambda ctx val AsImplLambda lam (IO [String])
  ) =>
  EvalLambda
    ctx val
    AsImplLambda
    ('LambdaWrapper lam)
    (IO [String]) where
  evalLambda ctx val _ _ =
    evalLambda ctx val AsImplLambda $ Proxy @lam

instance
  ( Context ctx
  , KnownSymbol name
  , Dyn.Typeable valT
  , EvalLambda ctx valT AsImplLambda lam (IO [String])
  ) =>
  Eval ctx AsImplAction
       (GetPayloadValueImpl ('TagWrapper name t valT) lam)
       (IO [String]) where
  eval ctx _ _ = do
    let key = symbolVal $ Proxy @name
    withContextValue ctx key
      $ \(val :: valT) -> evalLambda ctx val AsImplLambda $ Proxy @lam
    pure []

instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable valT
  , EvalLambda ctx valT AsImplLambda lam (IO [String])
  )
  => Eval ctx AsImplAction (ReadRefImpl valT refName lam) (IO [String]) where
  eval ctx _ _ = do
    let valName = symbolVal $ Proxy @refName
    withContextValue ctx valName
      $ \(val :: valT) -> evalLambda ctx val AsImplLambda $ Proxy @lam
    pure []

instance
  ( Show val
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  ) =>
  EvalLambda ctx val
  AsImplLambda
  (ShowFImpl ('LambdaWrapper lam))
  (IO [String]) where
  evalLambda ctx val _ _ = do
    let valStr = show val
    evalLambda ctx valStr AsImplLambda $ Proxy @lam

instance
  ( Context ctx
  , EvalLambda ctx val AsImplLambda lam1 (IO [String])
  , EvalLambda ctx val AsImplLambda lam2 (IO [String])
  ) =>
  EvalLambda
    ctx val
    AsImplLambda
    (BothImpl lam1 lam2)
    (IO [String]) where
  evalLambda ctx val _ _ = do
    evalLambda ctx val AsImplLambda (Proxy :: Proxy lam1)
    evalLambda ctx val AsImplLambda (Proxy :: Proxy lam2)
    pure []

instance
  ( Show val
  ) =>
  EvalLambda ctx val AsImplLambda PrintFImpl (IO [String]) where
  evalLambda _ val _ _ = pure [show val]

instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  ) =>
  EvalLambda ctx String
    AsImplLambda
    (ConcatLImpl str lam)
    (IO [String]) where
  evalLambda ctx val _ _ = do
    let lStr = symbolVal (Proxy :: Proxy str)
    evalLambda ctx (lStr ++ val) AsImplLambda (Proxy :: Proxy lam)

instance
  ( Context ctx
  , KnownSymbol str
  , EvalLambda ctx String AsImplLambda lam (IO [String])
  ) =>
  EvalLambda ctx String
    AsImplLambda
    (ConcatRImpl lam str)
    (IO [String]) where
  evalLambda ctx val _ _ = do
    let rStr = symbolVal $ Proxy @str
    evalLambda ctx (val ++ rStr) AsImplLambda $ Proxy @lam

instance
  ( Context ctx
  , KnownSymbol refName
  , Dyn.Typeable t
  ) =>
  EvalLambda ctx t
    AsImplLambda
    (WriteRefImpl valT refName)
    (IO [String]) where
  evalLambda ctx val _ _ = do
    let valName = symbolVal $ Proxy @refName
    let dynVal = Dyn.toDyn val
    setDyn ctx valName dynVal
    pure []

withContextValue
  :: forall valT ctx a
   . Context ctx
  => Dyn.Typeable valT
  => ctx
  -> String
  -> (valT -> IO a)
  -> IO a
withContextValue ctx valName f = do
    mbDyn <- getDyn ctx valName
    case mbDyn of
      Nothing -> error $ "Value " <> valName <> " not found."
      Just dyn -> case Dyn.fromDynamic dyn of
        Nothing -> error $ "Value " <> valName <> " not parsed."
        Just (val :: valT) -> do
          putStrLn $ "Successfully fetched value " <> valName <> "."
          f val

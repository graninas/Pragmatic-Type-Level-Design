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

module Auction.Introspection.DataActions where

import Auction.Language
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsIntroAction = AsIntroAction
data AsIntroLambda = AsIntroLambda

instance Eval () AsIntroAction '[] (IO [String]) where
  eval () _ _ = pure []

instance
  ( Eval () AsIntroAction act (IO [String])
  , Eval () AsIntroAction acts (IO [String])
  ) =>
  Eval () AsIntroAction
    (ActionWrapper act ': acts)
    (IO [String]) where
  eval () _ _ = do
    strs1 <- eval () AsIntroAction $ Proxy @act
    strs2 <- eval () AsIntroAction $ Proxy @acts
    pure $ strs1 <> strs2



instance
  ( KnownSymbol refName
  ) =>
  EvalLambda () ()
    AsIntroLambda
    (WriteRefImpl rtType refName)
    (IO [String]) where
  evalLambda _ _ _ _ =
    pure ["WriteRefImpl reached", symbolVal $ Proxy @refName]

instance
  EvalLambda () ()
    AsIntroLambda
    PrintFImpl
    (IO [String]) where
  evalLambda _ _ _ _ =
    pure ["PrintFImpl reached"]

instance
  ( EvalLambda () () AsIntroLambda lam (IO [String])
  ) =>
  EvalLambda () ()
    AsIntroLambda
    (ShowFImpl lam)
    (IO [String]) where
  evalLambda _ _ _ _ = do
    strs <- evalLambda () () AsIntroLambda $ Proxy @lam
    pure $ "ShowFImpl reached" : strs

instance
  ( EvalLambda () () AsIntroLambda lam (IO [String])
  ) =>
  EvalLambda () ()
    AsIntroLambda
    ('LambdaWrapper lam)
    (IO [String]) where
  evalLambda _ _ _ _ =
    evalLambda () () AsIntroLambda $ Proxy @lam

instance
  ( EvalLambda () () AsIntroLambda lam (IO [String])
  ) =>
  Eval ()
    AsIntroAction
    (GetPayloadValueImpl tag lam)
    (IO [String]) where
    eval _ _ _ = do
      strs <- evalLambda () () AsIntroLambda $ Proxy @lam
      pure $ "GetPayloadValueImpl reached" : strs


instance
  ( KnownSymbol str
  , EvalLambda () () AsIntroLambda lam (IO [String])
  ) =>
  EvalLambda () ()
    AsIntroLambda
    (ConcatLImpl str lam)
    (IO [String]) where
  evalLambda _ _ _ _ = do
    let str = symbolVal $ Proxy @str
    strs <- evalLambda () () AsIntroLambda $ Proxy @lam
    pure $ "ConcatLImpl reached" : str : strs

instance
  ( KnownSymbol str
  , EvalLambda () () AsIntroLambda lam (IO [String])
  ) =>
  EvalLambda () ()
    AsIntroLambda
    (ConcatRImpl str lam)
    (IO [String]) where
  evalLambda _ _ _ _ = do
    let str = symbolVal $ Proxy @str
    strs <- evalLambda () () AsIntroLambda $ Proxy @lam
    pure $ "ConcatRImpl reached" : str : strs

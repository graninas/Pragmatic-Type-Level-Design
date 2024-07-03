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
{-# LANGUAGE AllowAmbiguousTypes      #-}
-- {-# LANGUAGE QuantifiedConstraints    #-}
-- {-# LANGUAGE ImpredicativeTypes    #-}

module TypeLevelDSL.Implementation.Action where

import TypeLevelDSL.Language
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)


data AsImplAction = AsImplAction
data AsImplActions = AsImplActions
data AsImplLambda = AsImplLambda

-- The Actions mechanism

instance
  ( KnownSymbol name
  , mkAct ~ (MkAction (ReadRef' name Int (MkLambda (WriteRef' "val2" Int))))
  ) =>
  EvalCtx ctx AsImplAction
    mkAct
    (IO [String]) where
  evalCtx _ _ _ = pure []


instance
  EvalCtx ctx AsImplActions '[] (IO [String]) where
  evalCtx _ _ _ = pure []

instance
  ( EvalCtx ctx AsImplActions acts (IO [String])
  , EvalCtx ctx AsImplAction act (IO [String])
  , act ~ (x :: IAction)
  ) =>
  EvalCtx ctx AsImplActions (act ': acts) (IO [String]) where
  evalCtx ctx _ _ = do
    strs1 <- evalCtx ctx AsImplAction (Proxy @act)
    -- strs2 <- evalCtx ctx AsImplActions (Proxy @acts)
    -- pure $ strs1 <> strs2
    pure strs1

instance
  ( acts ~ (b :: [IAction])
  , EvalCtx ctx AsImplActions acts (IO [String])
  ) =>
  EvalCtx ctx AsImplAction acts (IO [String]) where
  evalCtx ctx _ _ =
    evalCtx ctx AsImplActions (Proxy @acts)


-- Lambda mechanism

instance
  ( mkLam ~ MkLambda lam
  , EvalLambdaCtx ctx valType AsImplLambda lam (IO [String])
  ) =>
  EvalLambdaCtx ctx valType AsImplLambda mkLam (IO [String]) where
  evalLambdaCtx ctx val _ _ =
    evalLambdaCtx ctx val AsImplLambda (Proxy :: Proxy lam)

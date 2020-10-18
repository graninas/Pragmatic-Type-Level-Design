{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}

-- instance (Eval AsEngine engine (), Eval AsPart parts ()) =>
--          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleContexts         #-}

-- instance (b ~ Parts a, Eval AsPart a ()) => Eval AsPart b ()
--                                             ^^^^^^^^^^^^^^^^
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Car.Implementation where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol)

import TypeLevelDSL.Car.Language
import TypeLevelDSL.Eval

-- Interpreting of the (engine :: EngineTag x)
data AsEngine = AsEngine

instance (b ~ Engine a, Eval AsEngine a ()) => Eval AsEngine b () where
  eval _ _ = eval AsEngine (Proxy :: Proxy a)


-- Interpreting of the (parts :: PartsTag a)

data AsPart = AsPart

instance Eval AsPart '[] () where
  eval _ _ = pure ()

instance Eval AsPart p () => Eval AsPart (p ': '[]) () where
  eval _ _ = eval AsPart (Proxy :: Proxy p)

instance (Eval AsPart p (), Eval AsPart (x ': ps) ()) => Eval AsPart (p ': x ': ps) () where
  eval _ _ = do
    eval AsPart (Proxy :: Proxy p)
    eval AsPart (Proxy :: Proxy (x ': ps))

instance (b ~ Parts a, Eval AsPart a ()) => Eval AsPart b () where
  eval _ _ = eval AsPart (Proxy :: Proxy a)


-- Interpreting of the Car

data AsCar = AsCar

instance (Eval AsEngine engine (), Eval AsPart parts ()) =>
  Eval AsCar (Car name engine parts) () where
  eval _ _ = do
    putStrLn "This is a car."
    eval AsEngine (Proxy :: Proxy engine)
    eval AsPart (Proxy :: Proxy parts)

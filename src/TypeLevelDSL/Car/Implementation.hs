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


-- Interpreting of the list (parts :: PartsTag a)

data AsParts = AsParts

-- Empty list of parts is allowed.
instance Eval AsParts '[] () where
  eval _ _ = pure ()

instance Eval AsParts p () => Eval AsParts (p ': '[]) () where
  eval _ _ = eval AsParts (Proxy :: Proxy p)

instance (Eval AsParts p (), Eval AsParts (x ': ps) ()) => Eval AsParts (p ': x ': ps) () where
  eval _ _ = do
    eval AsParts (Proxy :: Proxy p)
    eval AsParts (Proxy :: Proxy (x ': ps))

instance (b ~ Parts a, Eval AsParts a ()) => Eval AsParts b () where
  eval _ _ = eval AsParts (Proxy :: Proxy a)


-- Interpreting of the Car

data AsCar = AsCar

instance (Eval AsEngine engine (), Eval AsPart parts ()) =>
  Eval AsCar (Car name engine parts) () where
  eval _ _ = do
    putStrLn "This is a car."
    eval AsEngine (Proxy :: Proxy engine)
    eval AsParts (Proxy :: Proxy parts)

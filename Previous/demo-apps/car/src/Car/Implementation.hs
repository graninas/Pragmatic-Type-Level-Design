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

module Car.Implementation where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import TypeLevelDSL.Eval

import Car.Language

-- Interpretation tags

data AsEngine = AsEngine
data AsParts = AsParts
data AsPart = AsPart
data AsCar = AsCar


-- Interpreting of the Engine extension (engine :: EngineTag x)

instance
  ( b ~ Engine a
  , Eval AsEngine a (IO ())
  ) =>
  Eval AsEngine b (IO ()) where
  eval _ _ = eval AsEngine (Proxy :: Proxy a)


-- Interpreting of the list (parts :: PartsTag a)

-- Empty list of parts is allowed.
instance Eval AsParts '[] (IO ()) where
  eval _ _ = pure ()

-- N.B., item is interpreted AsPart
instance Eval AsPart p (IO ()) =>
  Eval AsParts (p ': '[]) (IO ()) where
  eval _ _ = eval AsPart (Proxy :: Proxy p)

-- N.B., item is interpreted AsPart
instance
  ( Eval AsPart p (IO ())
  , Eval AsParts (x ': ps) (IO ())
  ) =>
  Eval AsParts (p ': x ': ps) (IO ()) where
  eval _ _ = do
    eval AsPart (Proxy :: Proxy p)
    eval AsParts (Proxy :: Proxy (x ': ps))


-- Interpreting of the Parts extension

instance
  ( b ~ Parts a
  , Eval AsParts a (IO ())
  ) =>
  Eval AsParts b (IO ()) where
  eval _ _ = eval AsParts (Proxy :: Proxy a)


-- Interpreting of the Car

instance
  ( KnownSymbol name
  , Eval AsEngine engine (IO ())
  , Eval AsParts parts (IO ())
  ) =>
  Eval AsCar (Car name engine parts) (IO ()) where
  eval _ _ = do
    putStrLn $ "This is a car: " <> symbolVal (Proxy :: Proxy name)
    eval AsEngine (Proxy :: Proxy engine)
    eval AsParts (Proxy :: Proxy parts)

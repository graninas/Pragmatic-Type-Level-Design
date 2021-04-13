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
import GHC.TypeLits (KnownSymbol, symbolVal)

import TypeLevelDSL.Car.Language
import TypeLevelDSL.Eval


-- Interpretation tags

data AsEngine = AsEngine
data AsParts = AsParts
data AsPart = AsPart
data AsCar = AsCar


-- Interpreting of the Engine extension (engine :: EngineTag x)

instance (b ~ Engine a, Eval AsEngine a ()) =>
  Eval AsEngine b () where
  eval _ _ = eval AsEngine (Proxy :: Proxy a)


-- Interpreting of the list (parts :: PartsTag a)

-- Empty list of parts is allowed.
instance Eval AsParts '[] () where
  eval _ _ = pure ()

-- N.B., item is interpreted AsPart
instance Eval AsPart p () =>
  Eval AsParts (p ': '[]) () where
  eval _ _ = eval AsPart (Proxy :: Proxy p)

-- N.B., item is interpreted AsPart
instance (Eval AsPart p (), Eval AsParts (x ': ps) ()) =>
  Eval AsParts (p ': x ': ps) () where
  eval _ _ = do
    eval AsPart (Proxy :: Proxy p)
    eval AsParts (Proxy :: Proxy (x ': ps))


-- Interpreting of the Parts extension

instance (b ~ Parts a, Eval AsParts a ()) =>
  Eval AsParts b () where
  eval _ _ = eval AsParts (Proxy :: Proxy a)


-- Interpreting of the Car

instance (KnownSymbol name, Eval AsEngine engine (), Eval AsParts parts ()) =>
  Eval AsCar (Car name engine parts) () where
  eval _ _ = do
    putStrLn $ "This is a car: " <> symbolVal (Proxy :: Proxy name)
    eval AsEngine (Proxy :: Proxy engine)
    eval AsParts (Proxy :: Proxy parts)

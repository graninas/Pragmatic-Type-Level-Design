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

module TypeLevelDSL.Car.Language where

import GHC.TypeLits (Symbol)

-- eDSL

data Car (name :: Symbol) (engine :: EngineTag x) (parts :: PartsTag a)

data EngineTag a

data PartsTag a

-- Construction of extensions

type family Engine (a :: *) :: EngineTag a

type family Parts (p :: [*]) :: PartsTag p

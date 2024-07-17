{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Eval where

import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (Symbol)


-- | The Eval type class.
-- Allows to pattern match over a type level DSL,
-- while interpreting it into something real.
-- The FunDep is needed to simplify the return type inference.

class Eval payload verb noun ret
  | verb noun -> ret where
  eval :: payload -> verb -> Proxy noun -> ret

-- | A version for a shorter type class instances
class EvalIO payload verb noun ret
  | verb noun -> ret where
  evalIO :: payload -> verb -> Proxy noun -> IO ret

-- | Evaluate with an input parameter
-- (interpret a lambda-like combinator)
class EvalLambda payload input verb noun ret
  | verb noun -> ret where
  evalLambda :: payload -> input -> verb -> Proxy noun -> ret

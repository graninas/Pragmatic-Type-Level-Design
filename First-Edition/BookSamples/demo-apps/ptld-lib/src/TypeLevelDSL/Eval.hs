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

-- This FunDep is needed to simplify the return type inference.
class Eval tag payload ret
  | tag payload -> ret where
  eval :: tag -> Proxy payload -> ret

-- Eval with context.
-- Currently, the context should be passed as an argument.

class EvalCtx ctx tag payload ret
  | tag payload -> ret where
  evalCtx :: ctx -> tag -> Proxy payload -> ret

-- Eval lambda with context and input
-- Currently, the context should be passed as an argument.

class EvalLambdaCtx ctx input tag payload ret
  | tag payload -> ret where
  evalLambdaCtx :: ctx -> input -> tag -> Proxy payload -> ret

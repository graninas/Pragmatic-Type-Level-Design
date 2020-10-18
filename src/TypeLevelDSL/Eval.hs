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




-- This FunDep is needed to simplify the return type inference.
class Eval tag payload ret | tag payload -> ret where
  eval :: tag -> Proxy payload -> IO ret

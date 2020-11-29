{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.Language.Action where

import GHC.TypeLits (Symbol, Nat)

data ActionTag a
data LambdaTag a

type family MkAction      (a :: *) :: ActionTag a
type family MkLambda      (a :: *) :: LambdaTag a

data Action' (act :: ActionTag a) acts
data End'
data Print'
data Drop'

type Action act acts              = Action' act acts    -- Just a synonym
type End                          = MkAction End'
type Print                        = MkLambda Print'
type Drop                         = MkLambda Drop'

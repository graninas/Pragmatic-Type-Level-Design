{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module TypeLevelDSL.Auction.Language.DataActions where

import TypeLevelDSL.Auction.Language.Action

import GHC.TypeLits (Symbol, Nat)


-- data GetPayloadValueTag a
-- data FunctionTag a

-- type family MkSetRef (a :: *) :: ActionTag a
-- type family MkGetRef (a :: *) :: ActionTag a
--
-- -- type family MkGetPayloadValue (a :: *) :: GetPayloadValueTag a

--
--
-- data SetRef' (refName :: Symbol) (src :: FunctionTag a)
-- data GetRef' (refName :: Symbol) (src :: FunctionTag a)
--
-- data Action (act :: ActionTag a)
--
-- type SetRef refName src = Action (MkSetRef (SetRef' refName src))
-- type GetRef refName src = Action (MkGetRef (GetRef' refName src))


-- Should valName be a Symbol?
data GetPayloadValue' (valName :: *) (valType :: *) (lam :: LambdaTag lamBody)
data ReadRef' (refName :: Symbol) (t :: *) (lam :: LambdaTag lamBody)



-- Helpers

type GetPayloadValue n t lam = MkAction (GetPayloadValue' n t lam)
type ReadRef n t lam = MkAction (ReadRef' n t lam)

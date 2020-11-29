{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}



module TypeLevelDSL.Auction.Language.Flow where

import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, KnownNat, Nat, symbolVal)

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


data AuctionFlowTag a
data LotProcessTag a
data ActionTag a
data LambdaTag a

type family MkAuctionFlow (a :: *) :: AuctionFlowTag a
type family MkLotProcess  (a :: *) :: LotProcessTag a
type family MkAction      (a :: *) :: ActionTag a
type family MkLambda      (a :: *) :: LambdaTag a

data AuctionFlow'     (lotProcess :: LotProcessTag lp)
data LotProcess'      (startActions :: *)

-- Should valName be a Symbol?
data GetPayloadValue' (valName :: *) (valType :: *) (lam :: LambdaTag lamBody)
data Action'          (act :: ActionTag a) acts
data End'
data Print'
data Drop'

type AuctionFlow lotProcess       = MkAuctionFlow (AuctionFlow' lotProcess)
type LotProcess startActions      = MkLotProcess (LotProcess' startActions)
type GetPayloadValue n t lam      = MkAction (GetPayloadValue' n t lam)
type Action act acts              = Action' act acts    -- Just a synonym
type End                          = MkAction End'
type Print                        = MkLambda Print'
type Drop                         = MkLambda Drop'

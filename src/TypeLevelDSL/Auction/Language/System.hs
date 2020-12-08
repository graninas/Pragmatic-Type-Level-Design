{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}

module TypeLevelDSL.Auction.Language.System where

import GHC.TypeLits (Symbol)

data RefTagTag a

data RefTag' (refName :: Symbol) (refType :: *)

-- Construction

type family MkRefTag  (a :: *) :: RefTagTag a

-- Helpers

type RefTag refName refType = MkRefTag (RefTag' refName refType)

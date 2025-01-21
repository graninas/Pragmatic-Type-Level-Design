{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ServantLike where

import qualified Data.Map as Map
import GHC.TypeLits
import GHC.Generics
import Data.Proxy

data IDataType where
  DataTypeWrapper :: a -> IDataType

type family MkDataType a :: IDataType where
  MkDataType a = 'DataTypeWrapper a

data IFormat where
  FormatWrapper :: a -> IFormat

type family MkFormat a :: IFormat where
  MkFormat a = 'FormatWrapper a

data IClause where
  ClauseWrapper :: a -> IClause

type family MkClause a :: IClause where
  MkClause a = 'ClauseWrapper a


data CaptureImpl
  (name :: Symbol)
  (t :: IDataType)

type Capture name t =
  MkClause (CaptureImpl name t)

data QueryParamImpl
  (name :: Symbol)
  (t :: IDataType)

type QueryParam name t =
  MkClause (CaptureImpl name t)

data GetImpl
  (formats :: [IFormat])
  (t :: IDataType)

type Get formats t =
  MkClause (GetImpl formats t)

data NoClausesImpl
type NoClauses = MkClause NoClausesImpl

data CompoundClauseImpl
  (path :: Symbol)
  (clause :: IClause)
  (restClauses :: IClause)

type CompoundClause path clause =
  MkClause (CompoundClauseImpl path clause NoClauses)


data PostImpl
  (formats :: [IFormat])
  (t :: IDataType)

type Post formats t =
  MkClause (PostImpl formats t)


data JsonImpl
type JSON = MkFormat JsonImpl

data DataTypeImpl d
type DataType d = MkDataType (DataTypeImpl d)


data IMethod where
  MethodWrapper :: a -> IMethod

type family MkMethod a :: IMethod where
  MkMethod a = 'MethodWrapper a

data MethodImpl
  (path :: Symbol)
  (clauses :: [IClause])

type Method path clauses =
  MkMethod (MethodImpl path clauses)



--- user-defined data

data User = User { userId :: Int, userName :: String }
  deriving (Show, Generic)


type UsersMethod = Method
  "users"
  '[ Capture "userId" (DataType Int)
   , Get '[JSON] (DataType User)
   ]

type SearchMethod = Method
  "search"
  '[ QueryParam "name" (DataType String)
   , Get '[JSON] (DataType [User])
   ]

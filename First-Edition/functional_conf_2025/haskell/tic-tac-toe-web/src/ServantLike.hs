{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ServantLike where

import qualified Data.Map as Map
import GHC.TypeLits
import GHC.Generics


data IDataType where
  DataTypeWrapper :: a -> IDataType

type family MkDataType a :: IDataType where
  MkDataType a = 'DataTypeWrapper a

data IFormat where
  FormatWrapper :: a -> IFormat

type family MkFormat a :: IFormat where
  MkFormat a = 'FormatWrapper a

data IServantLikeClause where
  ServantLikeClauseWrapper :: a -> IServantLikeClause

type family MkServantLikeClause a :: IServantLikeClause where
  MkServantLikeClause a = 'ServantLikeClauseWrapper a

data IServantLikeMethod where
  ServantLikeMethodWrapper :: a -> IServantLikeMethod

type family MkServantLikeMethod a :: IServantLikeMethod where
  MkServantLikeMethod a = 'ServantLikeMethodWrapper a


data CaptureImpl
  (name :: Symbol)
  (t :: IDataType)

type Capture name t =
  MkServantLikeClause (CaptureImpl name t)

data QueryParamImpl
  (name :: Symbol)
  (t :: IDataType)

type QueryParam name t =
  MkServantLikeClause (CaptureImpl name t)

data GetImpl
  (formats :: [IFormat])
  (t :: IDataType)

type Get formats t =
  MkServantLikeClause (GetImpl formats t)

data PostImpl
  (formats :: [IFormat])
  (t :: IDataType)

type Post formats t =
  MkServantLikeClause (PostImpl formats t)


data JsonImpl
type JSON = MkFormat JsonImpl

data DataTypeImpl d
type DataType d = MkDataType (DataTypeImpl d)

data ServantLikeMethodImpl
  (path :: Symbol)
  (clauses :: [IServantLikeClause])

type ServantLikeMethod path clauses =
  MkServantLikeMethod (ServantLikeMethodImpl path clauses)

--- user-defined data

data User = User { userId :: Int, userName :: String }
  deriving (Show, Generic)


type UsersMethod = ServantLikeMethod
  "users"
  '[ Capture "userId" (DataType Int)
   , Get '[JSON] (DataType User)
   ]

type SearchMethod = ServantLikeMethod
  "search"
  '[ QueryParam "name" (DataType String)
   , Get '[JSON] (DataType [User])
   ]

-- type UsersAPI =
--   (
--     "users"
--     :> Capture "userId" (DataType Int)
--     :> Get '[JSON] (DataType User)
--   )
--   :<|>
--   (
--     "search"
--     :> QueryParam "name" (DataType String)
--     :> Get '[JSON] (DataType [User])
--   )

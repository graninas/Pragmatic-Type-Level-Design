{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Map as Map
import GHC.TypeLits

import qualified ServantLike

data IDataType where
  DataTypeWrapper :: a -> IDataType

type family MkDataType a :: IDataType where
  MkDataType a = 'DataTypeWrapper a

data DataTypeImpl d
type DataType d = MkDataType (DataTypeImpl d)


data IFormat where
  FormatWrapper :: a -> IFormat

type family MkFormat a :: IFormat where
  MkFormat a = 'FormatWrapper a

data JsonImpl
type JSON = MkFormat JsonImpl

data IParam where
  ParamWrapper :: a -> IParam

type family MkParam a :: IParam where
  MkParam a = 'ParamWrapper a


data QueryParamImpl
  (name :: Symbol)
  (t :: IDataType)

type QueryParam name t =
  MkParam (QueryParamImpl name t)

data CaptureImpl
  (name :: Symbol)
  (t :: IDataType)

type Capture name t =
  MkParam (CaptureImpl name t)




data IMethod where
  MethodWrapper :: a -> IMethod

type family MkMethod a :: IMethod where
  MkMethod a = 'MethodWrapper a

data PostMethodImpl
  (supportedFormats :: [IFormat])
  (returnType :: IDataType)

type Post format ret =
  MkMethod (PostMethodImpl format ret)

data GetMethodImpl
  (supportedFormats :: [IFormat])
  (returnType :: IDataType)

type Get format ret =
  MkMethod (GetMethodImpl format ret)

data CustomRoute = Route
  { rPath :: Symbol
  , rParams :: [IParam]
  , rMethod :: IMethod
  }

data CustomAPI = API [CustomRoute]


data Game = Game
  { gGameId :: String
  }

data Board = Board (Map.Map (Int, Int) String)

type StartRoute = Route
  "/start"
  '[]
  (Post '[JSON] (DataType Game))

type MoveRoute = Route
  "/move"
  '[ Capture "id" (DataType String)
   , Capture "sign" (DataType String)
   , QueryParam "h" (DataType Int)
   , QueryParam "v" (DataType Int)
   ]
  (Post '[JSON] (DataType String))

type BoardRoute = Route
  "/board"
  '[ Capture "id" (DataType String) ]
  (Get '[JSON] (DataType Board))

type TicTacToeAPI = API
  '[ StartRoute
   , MoveRoute
   , BoardRoute
   ]




main :: IO ()
main = do
  putStrLn "Hello"

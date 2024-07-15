module Minefield.Core.Object where

import CPrelude

import Minefield.Core.Types

import GHC.TypeLits


data ObjectInfo = ObjectInfo
  { oiIcon :: Char
  , oiPos  :: Pos
  , oiObjectType :: ObjectType
  }

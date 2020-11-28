module TypeLevelDSL.Auction.Implementation.Types where

import qualified TypeLevelDSL.Auction.Types as T
import Data.IORef

data Lot = Lot
  { name        :: String
  , description :: String
  , currentCost :: IORef T.Money
  }

type Lots = Lots

data Payload = Payload
  { startBid :: T.Money
  }

data Bid = Bid T.Money
  deriving (Show)

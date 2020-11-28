{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}

module TypeLevelDSL.Auction.Implementation.Auction
  ( module Impl
  , AsImplAuction (..)
  , runAuction
  ) where

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Description.Language as L
import qualified TypeLevelDSL.Auction.Flow.Language as L
import qualified TypeLevelDSL.Auction.Language as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import qualified TypeLevelDSL.Auction.Implementation.Description as Impl
-- import qualified TypeLevelDSL.Auction.Implementation.Flow as Impl
import qualified TypeLevelDSL.Auction.Description.Introspection as I
import TypeLevelDSL.Eval

import Data.Proxy (Proxy(..))
import Data.IORef
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Control.Monad
import System.Random (randomIO, randomRIO)

-- Interpretation tags

data AsImplAuction = AsImplAuction

-- Interpreting of the Auction

for_ :: Monad m => [a] -> (a -> m b) -> m ()
for_ = flip mapM_

type Order = Int
type ParticipantNumber = Int

data Participant = Participant
  { participantNumber   :: ParticipantNumber
  , participantActivity :: IORef Int
  }


getParticipantDecision :: T.Money -> Participant -> IO (Maybe (ParticipantNumber, Order))
getParticipantDecision _ (Participant pNum actRef) = do
  x <- randomRIO (1, 100)    -- Activity decreasing value
  o <- randomRIO (1, 1000)   -- hardcoded value to simulate the priority of a bid
  curDecision <- randomRIO (1, x)

  act <- readIORef actRef
  let newAct = act - x
  writeIORef actRef newAct
  if (newAct > 0 && curDecision >= 50)
    then pure $ Just (pNum, o)
    else pure Nothing

registerParticipants :: IO [Participant]
registerParticipants = mapM createParticipant [1..3]
  where
    createParticipant pNum = do
      baseAct <- randomRIO (400, 1000)
      actRef <- newIORef baseAct
      pure $ Participant pNum actRef


-- Tmp harcode
lotRounds :: Int
lotRounds = 3

costIncrease :: T.Money
costIncrease = 500

instance
  ( Eval Impl.AsImplLots lots Impl.Lots
  , Eval I.AsIntroLots lots [String]
  -- , Eval Impl.AsImplAuctionFlow flow ()
  ) =>
  Eval AsImplAuction (L.Auction' flow info lots) () where
  eval _ _ = do

    -- "participants registration".

    putStrLn "Auction is started."

    lots       <- eval Impl.AsImplLots (Proxy :: Proxy lots)
    lotsDescrs <- eval I.AsIntroLots (Proxy :: Proxy lots)

    for_ (zip lots lotsDescrs) $ \(lot, descr) -> do
      putStrLn "New lot!"
      putStrLn descr
      participants <- registerParticipants
      lotProcess lotRounds Nothing participants lot

finalizeLot :: Maybe ParticipantNumber -> Impl.Lot -> IO ()
finalizeLot Nothing     lot = putStrLn $ "Lot " <> Impl.name lot <> " is not sold."
finalizeLot (Just pNum) lot = putStrLn $ "Lot " <> Impl.name lot <> " goes to the participant " <> show pNum <> "."

lotProcess :: Int -> Maybe ParticipantNumber -> [Participant] -> Impl.Lot -> IO ()
lotProcess 0 curOwner _  lot = finalizeLot curOwner lot
lotProcess n curOwner ps lot = do
  putStrLn $ "Round: " <> show (4 - n)

  lastCost <- readIORef $ Impl.currentCost lot
  let newCost = lastCost + costIncrease
  putStrLn $ "Current lot cost: " <> show newCost <> ". Who will pay?"

  rawDecisions <- mapM (getParticipantDecision newCost) ps
  let decisions = sortOn snd $ catMaybes rawDecisions
  putStrLn $ "Raw decisions: " <> show rawDecisions

  case decisions of
    [] -> lotProcess (n - 1) curOwner ps lot
    ((pNum,_) : _) -> do
      putStrLn $ "Current owner is participant " <> show pNum <> "."
      writeIORef (Impl.currentCost lot) newCost
      lotProcess lotRounds (Just pNum) ps lot


runAuction
  :: Eval AsImplAuction auction ()
  => Proxy auction
  -> IO ()
runAuction p = eval AsImplAuction p

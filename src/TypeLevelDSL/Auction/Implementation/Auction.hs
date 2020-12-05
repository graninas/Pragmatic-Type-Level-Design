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
import qualified TypeLevelDSL.Auction.Language.Description as L
import qualified TypeLevelDSL.Auction.Language.Flow as L
import qualified TypeLevelDSL.Auction.Language.Auction as L
import qualified TypeLevelDSL.Auction.Implementation.Types as Impl
import qualified TypeLevelDSL.Auction.Implementation.Description as Impl
import qualified TypeLevelDSL.Auction.Implementation.Flow as Impl
import qualified TypeLevelDSL.Auction.Implementation.Action as Impl
import qualified TypeLevelDSL.Auction.Implementation.DataActions as Impl
import qualified TypeLevelDSL.Auction.Introspection.Description as I
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

data LotState = LotState
  { lotNameRef  :: IORef String
  , curRoundRef :: IORef Int
  , curOwnerRef :: IORef (Maybe ParticipantNumber)
  , curCostRef  :: IORef T.Money
  }

data AuctionState = AuctionState
  { participants :: [Participant]
  , lotRounds    :: Int
  , costIncrease :: T.Money
  , lotState     :: LotState
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

initParticipants :: AuctionState -> IO ()
initParticipants (AuctionState {..}) =
  mapM_ initParticipant participants
  where
    initParticipant (Participant {..}) = pure ()
      -- baseAct <- randomRIO (400, 1000)
      -- actRef <- newIORef baseAct
      -- pure $ Participant pNum actRef

initLot :: AuctionState -> Impl.Lot -> IO ()
initLot (AuctionState {..}) lot = do
  let LotState {..} = lotState
  writeIORef lotNameRef $ Impl.name lot
  writeIORef curRoundRef lotRounds
  writeIORef curOwnerRef Nothing

instance
  ( Eval Impl.AsImplLots lots Impl.Lots
  , Eval I.AsIntroLots lots [String]
  , Eval Impl.AsImplAuctionFlow flow Impl.AuctionFlow
  ) =>
  Eval AsImplAuction (L.Auction' flow info lots) () where
  eval _ _ = do

    auctionState <- AuctionState
      <$> registerParticipants
      <*> pure 3
      <*> pure 500
      <*> (LotState
            <$> newIORef ""
            <*> newIORef 0
            <*> newIORef Nothing
            <*> newIORef 0)

    putStrLn "Auction is started."

    lots        <- eval Impl.AsImplLots (Proxy :: Proxy lots)
    auctionFlow <- eval Impl.AsImplAuctionFlow (Proxy :: Proxy flow)
    lotsDescrs  <- eval I.AsIntroLots (Proxy :: Proxy lots)

    for_ (zip lots lotsDescrs) $ \(lot, descr) -> do
      initParticipants auctionState
      initLot auctionState lot
      -- auctionFlow (lot, descr)
      putStrLn "New lot!"
      putStrLn descr
      lotProcess' auctionState lot

finalizeLot' :: LotState -> IO ()
finalizeLot' LotState {..} = do
  curOwner <- readIORef curOwnerRef
  lotName  <- readIORef lotNameRef
  case curOwner of
    Nothing   -> putStrLn $ "Lot " <> lotName <> " is not sold."
    Just pNum -> putStrLn $ "Lot " <> lotName <> " goes to the participant " <> show pNum <> "."

lotProcess' :: AuctionState -> Impl.Lot -> IO ()
lotProcess' st@(AuctionState {..}) lot = do
  let LotState {..} = lotState

  curRound <- readIORef curRoundRef
  curCost  <- readIORef curCostRef
  let newCost = curCost + costIncrease

  putStrLn $ "Round: " <> show curRound
  case curRound of
    0 -> finalizeLot' lotState
    _ -> do
      putStrLn $ "New lot cost: " <> show newCost <> ". Who will pay?"

      rawDecisions <- mapM (getParticipantDecision newCost) participants
      let decisions = sortOn snd $ catMaybes rawDecisions
      putStrLn $ "Raw decisions: " <> show rawDecisions

      case decisions of
        []  -> do
          writeIORef curRoundRef $ curRound - 1
          lotProcess' st lot
        ((pNum,_) : _) -> do
          putStrLn $ "Current owner is participant " <> show pNum <> "."
          writeIORef curCostRef newCost
          writeIORef curOwnerRef $ Just pNum
          writeIORef curRoundRef lotRounds
          lotProcess' st lot

runAuction
  :: Eval AsImplAuction auction ()
  => Proxy auction
  -> IO ()
runAuction p = eval AsImplAuction p

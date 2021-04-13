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
-- {-# LANGUAGE AllowAmbiguousTypes      #-}

module TypeLevelDSL.Auction.Implementation.Auction
  ( module Impl
  , AsImplAuction (..)
  , runAuction
  ) where

import qualified TypeLevelDSL.Auction.Types as T
import qualified TypeLevelDSL.Auction.Language.Description as L
import qualified TypeLevelDSL.Auction.Language.Flow as L
import qualified TypeLevelDSL.Auction.Language.Auction as L
import qualified TypeLevelDSL.Auction.Implementation.Description as Impl
import qualified TypeLevelDSL.Auction.Implementation.Flow as Impl
import qualified TypeLevelDSL.Auction.Implementation.Action as Impl
import qualified TypeLevelDSL.Auction.Implementation.DataActions as Impl
import TypeLevelDSL.Eval
import TypeLevelDSL.Context
import TypeLevelDSL.StateContext

import qualified Data.Dynamic as Dyn (toDyn)
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
  , lotPayloadContext :: StateContext
  }

data AuctionState = AuctionState
  { participants :: [Participant]
  , lotRounds    :: Int
  , costIncrease :: T.Money
  , lotState     :: LotState
  }

instance Context AuctionState where
  getDyn AuctionState {lotState} "curRound" _ = readIORef (curRoundRef lotState) >>= mkVal
  getDyn AuctionState {lotState} "curCost"  _ = readIORef (curCostRef lotState) >>= mkVal
  getDyn _ _ _ = noVal
  setDyn AuctionState {lotState} "curRound" val _ = error "setDyn for curRound not implemented"
  setDyn AuctionState {lotState} "curCost" val _ = error "setDyn for curCost not implemented"
  setDyn AuctionState {lotState} n val _ = error $ "setDyn for " <> n <> " not implemented"

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

initLotState :: AuctionState -> Impl.LotDescr -> IO ()
initLotState (AuctionState {..}) lotDescr = do
  let LotState {..} = lotState
  writeIORef lotNameRef $ Impl.ldName lotDescr
  writeIORef curRoundRef lotRounds
  writeIORef curOwnerRef Nothing
  writeIORef curCostRef 0               --   hardcode, should be extracted from payload

instance
  ( Eval Impl.AsImplLots lots Impl.LotDescrs
  -- , EvalCtx AuctionState Impl.AsImplAuctionFlow flow Impl.AuctionFlow
  ) =>
  Eval AsImplAuction (L.Auction' flow info lots) () where
  eval _ _ = do

    auctionState <- AuctionState
      <$> registerParticipants
      <*> pure 3      -- Number of rounds
      <*> pure 500    -- Bid increase     -- should be extension (increase / decrease)
      <*> (LotState
            <$> newIORef ""         -- Lot name
            <*> newIORef 0          -- Current round
            <*> newIORef Nothing    -- Current owner
            <*> newIORef 0          -- Current cost
            <*> createStateContext  -- Payload context
            )

    putStrLn "Auction is started."

    let ctx = auctionState

    lotDescrs   <- eval Impl.AsImplLots (Proxy :: Proxy lots)
    -- auctionFlow <- evalCtx ctx Impl.AsImplAuctionFlow (Proxy :: Proxy flow)

    for_ lotDescrs $ \lotDescr -> do
      putStrLn $ "New lot: " <> Impl.ldName lotDescr
      putStrLn $ Impl.ldDescription lotDescr

      initParticipants auctionState
      initLotState auctionState lotDescr
      lotProcess' auctionState

finalizeLot' :: LotState -> IO ()
finalizeLot' LotState {..} = do
  curOwner <- readIORef curOwnerRef
  lotName  <- readIORef lotNameRef
  case curOwner of
    Nothing   -> putStrLn $ "Lot " <> lotName <> " is not sold."
    Just pNum -> putStrLn $ "Lot " <> lotName <> " goes to the participant " <> show pNum <> "."

lotProcess' :: AuctionState -> IO ()
lotProcess' st@(AuctionState {..}) = do
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
          lotProcess' st
        ((pNum,_) : _) -> do
          putStrLn $ "Current owner is participant " <> show pNum <> "."
          writeIORef curCostRef newCost
          writeIORef curOwnerRef $ Just pNum
          writeIORef curRoundRef lotRounds
          lotProcess' st

runAuction
  :: Eval AsImplAuction auction ()
  => Proxy auction
  -> IO ()
runAuction p = eval AsImplAuction p

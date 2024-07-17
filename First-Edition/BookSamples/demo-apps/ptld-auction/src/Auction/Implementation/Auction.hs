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
{-# LANGUAGE AllowAmbiguousTypes      #-}

module Auction.Implementation.Auction where

import qualified Auction.Types as T
import qualified Auction.Language as L
import qualified Auction.Implementation.DataActions as Impl
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


-- Service types

data LotDescr = LotDescr
  { ldName           :: String
  , ldDescription    :: String
  , ldPayloadContext :: StateContext
  }

type LotDescrs = [LotDescr]

-- Interpretation tags

data AsImplInfo       = AsImplInfo
data AsImplLot        = AsImplLot
data AsImplLotPayload = AsImplLotPayload
data AsImplAuction    = AsImplAuction


-- Wrappers (extension points)

-- -- Lot payload

instance
  ( Eval () AsImplLotPayload p (IO StateContext)
  ) =>
  Eval () AsImplLotPayload
    (L.LotPayloadWrapper p)
    (IO StateContext) where
  eval () _ _ = eval () AsImplLotPayload $ Proxy @p

-- -- Lot wrapper

instance
  ( Eval () AsImplLot p (IO [LotDescr])
  ) =>
  Eval () AsImplLot (L.LotWrapper p) (IO [LotDescr]) where
  eval () _ _ = eval () AsImplLot $ Proxy @p

-- Instances

-- -- List of lots

instance
  ( Eval () AsImplLot lot (IO LotDescr)
  , Eval () AsImplLot lots (IO [LotDescr])
  ) =>
  Eval () AsImplLot (lot ': lots) (IO [LotDescr]) where
  eval () _ _ = do
    d  <- eval () AsImplLot $ Proxy @lot
    ds <- eval () AsImplLot $ Proxy @lots
    pure $ d : ds

instance
  Eval () AsImplLot '[] (IO [LotDescr]) where
  eval () _ _ = pure []

-- -- Lot

-- N.B., currency and censorship are not implemented yet
instance
  ( Eval () AsImplLotPayload payload (IO StateContext)
  , KnownSymbol name
  , KnownSymbol descr
  ) =>
  Eval () AsImplLot
    (L.LotImpl name descr payload currency_ censorship_)
    (IO LotDescr) where
  eval () _ _ = do
    payloadCtx <- eval () AsImplLotPayload $ Proxy @payload
    pure $ LotDescr
      { ldName           = symbolVal $ Proxy @name
      , ldDescription    = symbolVal $ Proxy @descr
      , ldPayloadContext = payloadCtx
      }

-----------------------------------------------------------
-- Auction implementation with some hardcoded parameters --
-----------------------------------------------------------

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
  getDyn AuctionState {lotState} "curRound" = mkVal <$> readIORef (curRoundRef lotState)
  getDyn AuctionState {lotState} "curCost"  = mkVal <$> readIORef (curCostRef lotState)
  getDyn _ _ = noVal
  setDyn _ "curRound" val = error "setDyn for curRound not implemented"
  setDyn _ "curCost" val = error "setDyn for curCost not implemented"
  setDyn _ n val = error $ "setDyn for " <> n <> " not implemented"
  getSubContext _ _ = error "Subcontext is not implemented."

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

initLotState :: AuctionState -> LotDescr -> IO ()
initLotState (AuctionState {..}) lotDescr = do
  let LotState {..} = lotState
  writeIORef lotNameRef $ ldName lotDescr
  writeIORef curRoundRef lotRounds
  writeIORef curOwnerRef Nothing
  writeIORef curCostRef 0               --   hardcode, should be extracted from payload

-- N.B., flow and info are not implemented yet
instance
  ( Eval () AsImplLot lots (IO LotDescrs)
  ) =>
  Eval () AsImplAuction
    (L.AuctionImpl flow_ info_ lots)
    (IO ()) where
  eval () _ _ = do

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

    lotDescrs   <- eval () AsImplLot $ Proxy @lots

    for_ lotDescrs $ \lotDescr -> do
      putStrLn $ "New lot: " <> ldName lotDescr
      putStrLn $ ldDescription lotDescr

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
  :: Eval () AsImplAuction auction (IO ())
  => Proxy auction
  -> IO ()
runAuction p = eval () AsImplAuction p

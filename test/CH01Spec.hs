{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module CH01Spec where

import BookSamples.CH01.S01_04

import           Test.Hspec
import           Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.Map as Map


type WalletAPIResult = Either String ()

type Wallets = IORef (Map.Map Wallet (Currency, Amount))


-- Errors:
-- * No check for invalid (negative, positive) amounts
-- * Inverted Deposit to Withdraw produces wrong error messages
-- * Shared access to bare IORef
-- * Lack of transactions
-- * No exception handling
-- * Withdrawing may success, but depositing may fail

-- Possible errors of interpreter:
-- * Invalid order of interpreting for act1 & act2 in the AndThen clause
-- * Missing checks for currency match
-- * Missing check for funds sufficiency
-- * Invalid math on amounts
-- * Missing Wallets update code

interpret :: Wallets -> WalletAPI a -> IO WalletAPIResult
interpret wsRef (AndThen act1 act2) = do
  act1Res <- interpret wsRef act1
  act2Res <- interpret wsRef act2
  pure $ act1Res >> act2Res
interpret wsRef (Withdraw w c a) = do
  ws <- readIORef wsRef
  case Map.lookup w ws of
    Nothing -> pure $ Left $ "Wallet " ++ w ++ " not found."
    Just (currency, curAmount) | currency /= c ->
      pure $ Left $
        "Invalid currency for " ++ w ++ ". "
        ++ currency ++ " expected but "
        ++ c ++ " got."
    Just (currency, curAmount) | curAmount < a ->
      pure $ Left $ "Insufficient funds for " ++ w ++ ". "
    Just (currency, curAmount) -> do
      writeIORef wsRef $ Map.insert w (currency, curAmount - a) ws
      pure $ Right ()
interpret wsRef (Deposit w c a) = interpret wsRef (Withdraw w c (0 - a))


-- Possible errors in the scenario:
-- * Mistakenly swapped wallets `from` and `to`
-- * Messed up currencies
-- * Messed up amounts
-- * Ignored / misplaced `currency`
-- * Ignored / misplaced `amount`
-- * Invalid order of Withdraw & Deposit operations
-- * Invalid math inbetween
-- * Duplicated Withdraw / Deposit operations
-- * Missing Withdraw / Deposit operation
transferMoney :: Wallet -> Wallet -> Currency -> Amount -> WalletAPI ()
transferMoney from to currency amount =
  (Withdraw from currency amount)
  `AndThen`
  (Deposit to currency amount)

-- Possible improvements
-- * Unifying Currency and Amount
-- * Transaction for Withdraw & Deposit
-- * Withdraw returns a token, Deposit is only possible with token instead of amount
-- * Unifying `Transfer` method
-- * `WalletTransaction` item containing both `from` & `to` wallets

spec :: Spec
spec =
  describe "Sample 01_4 test" $ do
    it "TransferMoney" $ do
      wallets <- newIORef Map.empty
      res <- interpret wallets $ transferMoney "MyWallet" "TheirWallet" "USD" 1000
      res `shouldBe` Left "Wallet MyWallet not found."

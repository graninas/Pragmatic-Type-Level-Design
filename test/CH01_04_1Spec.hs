{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module CH01_04_1Spec where

import BookSamples.CH01.S01_04_1

import           Test.Hspec
import           Data.Proxy (Proxy(..))
import Data.IORef
import qualified Data.Map as Map


type Result = Either String ()

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

interpret :: Wallets -> WalletAPI -> IO Result
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
-- * Confused currency & wallet
transferMoney :: Wallet -> Wallet -> Currency -> Amount -> WalletAPI
transferMoney from to currency amount =
  (Withdraw from currency amount)
  `AndThen`
  (Deposit to currency amount)

transferMoney2 :: Wallet -> Wallet -> Currency -> Amount -> WalletAPI
transferMoney2 from to currency amount =
  (Withdraw from currency amount
     `AndThen` Withdraw from currency amount)
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
  describe "Sample 01_4_1 test" $ do
    it "No wallets found" $ do
      wallets <- newIORef Map.empty
      res <- interpret wallets $ transferMoney "MyWallet" "TheirWallet" "USD" 1000
      res `shouldBe` Left "Wallet MyWallet not found."

    it "Successful transfer" $ do
      walletsRef <- newIORef $ Map.fromList [("MyWallet", ("USD", 2000)), ("TheirWallet", ("USD", 0))]
      res <- interpret walletsRef $ transferMoney "MyWallet" "TheirWallet" "USD" 1000
      wallets <- readIORef walletsRef
      let mbA1 = Map.lookup "MyWallet" wallets
      let mbA2 = Map.lookup "TheirWallet" wallets
      res `shouldBe` Right ()
      mbA1 `shouldBe` Just ("USD", 1000)
      mbA2 `shouldBe` Just ("USD", 1000)

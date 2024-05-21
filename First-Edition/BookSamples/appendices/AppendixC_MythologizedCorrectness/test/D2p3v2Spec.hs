{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module D2p3v2Spec where

import D2p3v2

import           Test.Hspec
import           Data.Proxy (Proxy(..))
import           Data.IORef
import qualified Data.Map as Map


type Wallets = IORef (Map.Map Wallet (Currency, Amount))

interpret :: Wallets -> WalletAPI a -> IO a
interpret wsRef (AndThen act1 fAct2) = do
  act1Res <- interpret wsRef act1
  case act1Res of
    Left err -> pure $ Left err
    Right val -> interpret wsRef $ fAct2 val
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
      pure $ Right $ TransactionToken currency a
interpret wsRef (Deposit w (TransactionToken tokCur tokAmount)) = do
  ws <- readIORef wsRef
  case Map.lookup w ws of
    Nothing -> pure $ Left $ "Wallet " ++ w ++ " not found."
    Just (currency, curAmount) | currency /= tokCur ->
      pure $ Left $
        "Invalid currency for " ++ w ++ ". "
        ++ currency ++ " expected but "
        ++ tokCur ++ " got."
    Just (currency, curAmount) -> do
      writeIORef wsRef $ Map.insert w (currency, curAmount + tokAmount) ws
      pure $ Right ()


transferMoney :: Wallet -> Wallet -> Currency -> Amount -> WalletAPI (Result ())
transferMoney from to currency amount =
  (Withdraw from currency amount)
  `AndThen`
  (\token -> Deposit to token)

-- Double Withdrawing is prohibited, this won't compile:
-- invalidDoubleWithdraw :: Wallet -> Wallet -> Currency -> Amount -> WalletAPI (Result ())
-- invalidDoubleWithdraw from to currency amount =
--   (Withdraw from currency amount                       -- 1st withdraw
--     `AndThen` (\_ -> Withdraw from currency amount     -- 2nd withdraw
--               `AndThen` (\token -> Deposit to token)
--               )
--   )
--   `AndThen` (\token -> Deposit to token)               -- no match: previous deposit returned ()

spec :: Spec
spec =
  describe "Sample 01_4_2 test" $ do
    it "TransferMoney, source wallet not found" $ do
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

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}

module BookSamples.CH01.S01_04 where

type Wallet = String
type Currency = String
type Amount = Int

data TransferMoney act where
  AndThen  :: TransferMoney act1 -> TransferMoney act2 -> TransferMoney ()
  Withdraw :: Wallet -> Currency -> Amount -> TransferMoney Amount
  Deposit  :: Wallet -> Currency -> Amount -> TransferMoney ()


transferMoney :: TransferMoney ()
transferMoney =
  (Withdraw "MyWallet" "USD" 1000)
  `AndThen`
  (Deposit "TheirWallet" "USD" 1000)

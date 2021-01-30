{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}

module BookSamples.CH01.S01_04 where

type Wallet = String
type Currency = String
type Amount = Int

data WalletAPI act where
  AndThen  :: WalletAPI act1 -> WalletAPI act2 -> WalletAPI ()
  Withdraw :: Wallet -> Currency -> Amount -> WalletAPI Amount
  Deposit  :: Wallet -> Currency -> Amount -> WalletAPI ()


-- See scenarios in [test/CH01Spec.hs]

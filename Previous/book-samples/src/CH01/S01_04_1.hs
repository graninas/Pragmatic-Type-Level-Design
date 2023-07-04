{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}

module CH01.S01_04_1 where

type Wallet = String
type Currency = String
type Amount = Int

data WalletAPI where
  AndThen  :: WalletAPI -> WalletAPI -> WalletAPI
  Withdraw :: Wallet -> Currency -> Amount -> WalletAPI
  Deposit  :: Wallet -> Currency -> Amount -> WalletAPI


-- See scenarios in [test/CH01_04_1Spec.hs]

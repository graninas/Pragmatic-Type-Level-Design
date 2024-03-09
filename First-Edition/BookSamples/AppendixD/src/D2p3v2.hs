{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}

module D2p3v2 where

type Wallet = String
type Currency = String
type Amount = Int

data TransactionToken = TransactionToken Currency Amount

type Result a = Either String a

data WalletAPI a where
  AndThen  :: WalletAPI (Result a) -> (a -> WalletAPI (Result b)) -> WalletAPI (Result b)
  Withdraw :: Wallet -> Currency -> Amount -> WalletAPI (Result TransactionToken)
  Deposit  :: Wallet -> TransactionToken -> WalletAPI (Result ())


-- See scenarios in [test/CH01_04_2Spec.hs]

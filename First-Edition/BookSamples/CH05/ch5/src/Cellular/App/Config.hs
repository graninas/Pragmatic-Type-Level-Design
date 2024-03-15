module Cellular.App.Config where


data ConfigRule = CfgRule
  { code :: String
  , defaultState :: String
  , transitionTable :: [String]
  }
  deriving (Read, Show, Eq, Ord)

data Config = Cfg
  { rule :: ConfigRule
  , renderer :: String
  }
  deriving (Read, Show, Eq, Ord)

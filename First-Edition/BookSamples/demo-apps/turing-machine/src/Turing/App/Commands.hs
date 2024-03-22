module Turing.App.Commands where


data Command
  = Help
  | Quit
  | Rules
  | Tapes
  | NewTape String
  | LoadRule String
  | LoadTape String
  | Run Int Int
  | PrintTape Int
  deriving (Show, Eq, Read, Ord)


printCommandsHelp :: IO ()
printCommandsHelp = do
  putStrLn "\nCommands:"
  putStrLn "Help  - this help message"
  putStrLn "Quit  - exit"
  putStrLn "Rules - list available rules"
  putStrLn "Tapes - list available tapes"
  putStrLn "NewTape contents - create tape from string"
  putStrLn "LoadRule path - load a dynamic rule"
  putStrLn "LoadTape path - load a tape"
  putStrLn "Run ruleIdx tapeIdx  - run a rule on a tape"
  putStrLn "Print tapeIdx - print a tape"

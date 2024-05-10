module Turing.App.Commands where


data Command
  = Help
  | Quit
  | Tapes
  | NewTape String
  | LoadTape String
  | LoadPredefTapes
  | Rules
  | LoadPredefRules
  | LoadRule String
  | Materialize
  | Run Int Int
  | PrintTape Int
  deriving (Show, Eq, Read, Ord)


printCommandsHelp :: IO ()
printCommandsHelp = do
  putStrLn "\nCommands:"
  putStrLn "Help                - this help message"
  putStrLn "Quit                - exit"
  putStrLn "Tapes               - list available tapes"
  putStrLn "NewTape contents    - create tape from string"
  putStrLn "LoadTape path       - load a tape"
  putStrLn "LoadPredefTapes     - load hardcoded tapes"
  putStrLn "Rules               - list available rules"
  putStrLn "LoadRule path       - load a rule"
  putStrLn "LoadPredefRules     - load hardcoded rules"
  putStrLn "Materialize         - Materialize static rules to dynamic ones\n  (for demo purposes)"
  putStrLn "Run ruleIdx tapeIdx - run a rule on a tape"
  putStrLn "Print tapeIdx       - print a tape"

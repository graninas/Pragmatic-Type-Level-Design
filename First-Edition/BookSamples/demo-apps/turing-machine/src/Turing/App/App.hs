module Turing.App.App where

import Turing.App.Storage
import Turing.App.Commands
import Turing.App.State
import Turing.App.Action ( AppAction, continue, continueWithMsg, finish )
import qualified Turing.App.Package.Rule as R
import Turing.Machine.Interface
import Turing.Machine.Language

import qualified Data.Map as Map
import Data.Proxy ( Proxy(..) )
import Data.IORef ( IORef, readIORef, writeIORef )
import Data.Traversable (for)
import Data.List (intercalate)
import Control.Exception ( SomeException, try )
import System.Directory
import Text.Read (readMaybe)


-- data Command
--   = Help
--   | Quit
--   | Rules
--   | Tapes
--   | LoadRule String
--   | LoadTape String
--   | Run Int Int
--   | PrintTape Int
--   deriving (Show, Eq, Read, Ord)

runCommand :: Command -> AppState -> IO AppAction
runCommand Help _ = printCommandsHelp >> continue
runCommand Quit _ = finish
runCommand Rules st = processListRules st
runCommand Tapes st = processListTapes st
runCommand (NewTape str) st = processNewTape st str
runCommand (PrintTape tapeIdx) st = processPrintTape st tapeIdx
runCommand (LoadRule str) st = processLoadRule st str
runCommand (Run ruleIdx tapeIdx) st = processRun st ruleIdx tapeIdx

-- printBoard :: Board -> IO ()
-- printBoard board = do
--     let ks :: [[Int]] = Map.keys board
--         xs :: [Int] = map (!! 0) ks
--         ys :: [Int] = map (!! 1) ks
--         minX = minimum xs
--         maxX = maximum xs
--         minY = minimum ys
--         maxY = maximum ys
--         printRow b coords@[_, x]
--             | x == maxX = putStrLn $ cellChar $ Map.findWithDefault 0 coords b
--             | otherwise = putStr   $ cellChar $ Map.findWithDefault 0 coords b
--         printRow _ _ = error "Only 2 dimension automata supported yet."
--         cellChar 0 = "."
--         cellChar 1 = "#"
--         cellChar n = error $ "Char not supported: " <> show n
--     mapM_ (printRow board) [[y, x] | y <- [minY..maxY], x <- [minX..maxX]]


-- loadFromFile2 :: FilePath -> IO Board
-- loadFromFile2 path = do
--   (content :: String) <- readFile path

--   let (rows :: [String]) = lines content
--   let (cells :: [[StateIdx]]) = map (map toCell) rows

--   pure (toBoard2 cells)
--   where
--     toCell :: Char -> StateIdx
--     toCell 'x' = 1
--     toCell _ = 0

-- toBoard2 :: [[StateIdx]] -> Board
-- toBoard2 cells
--   = Map.fromList
--   $ map (\((x,y), cell) -> ([x, y], cell))
--   $ toBoard2' cells

-- toBoard2' :: [[StateIdx]] -> [((Int, Int), StateIdx)]
-- toBoard2' cells = let
--   ixedCells :: [(Int, [(Int, StateIdx)])] = zip [1..] (map (zip [1..]) cells)
--   in foldr joinCells [] ixedCells
--   where
--     joinCells
--       :: (Int, [(Int, StateIdx)])
--       -> [((Int, Int), StateIdx)]
--       -> [((Int, Int), StateIdx)]
--     joinCells (i, rs) lst =
--       lst ++ map (\(j, cell) -> ((i, j), cell)) rs


-- loadWorld2
--   :: forall rule           -- Brings `rule` into the scope of body
--    . IAutomaton () rule    -- Demands the `rule` to be automaton.
--   => Proxy rule            -- Highlights what rule type was requrested by the caller.
--   -> FilePath
--   -> IO (Either String WorldInstance)
-- loadWorld2 _ path = do
--   eBoard <- try (loadFromFile2 path)
--   case eBoard of
--     Left (err :: SomeException) -> pure (Left (show err))
--     Right board -> pure (Right (WI @rule 0 (CW board)))    -- specifying the automaton rule


-- loadWorld2Dyn
--   :: FilePath
--   -> DynamicRule
--   -> IO (Either String WorldInstance)
-- loadWorld2Dyn path dynRule = do
--   eBoard <- try (loadFromFile2 path)
--   case eBoard of
--     Left (err :: SomeException) -> pure (Left (show err))
--     Right board -> pure (Right (DynWI dynRule 0 (CW board)))

-- -- App interface

processListRules :: AppState -> IO AppAction
processListRules (AppState rulesRef _) = do
  rules <- readIORef rulesRef
  putStrLn "\nSupported rules ([idx] name):"
  mapM_ f $ Map.toList rules
  continue
  where
    f (ruleIdx, RI proxy) =
      putStrLn ("[" <> show ruleIdx <> "] (static) " <> name () proxy)
    -- f (ruleIdx, DynRI dynRule) =
      -- putStrLn ("[" <> ruleIdx <> "] (dynamic) " <> name dynRule (Proxy @'DynRule))

printTape' :: (TapeIndex, Tape) -> IO ()
printTape' (idx, tape) = do
  putStrLn $ "[" <> show idx <> "] \"" <> printTape tape <> "\""


processListTapes :: AppState -> IO AppAction
processListTapes (AppState _ tapesRef) = do
  tapes <- readIORef tapesRef
  putStrLn ("\nTapes available: " <> show (Map.size tapes))
  let ts = Map.toAscList tapes
  mapM_ printTape' ts
  continue
    -- f :: (WorldIndex, WorldInstance) -> IO ()
    -- f (idx, WI gen cw) = f' idx gen cw
    -- f (idx, DynWI dynRule gen cw) = fDyn' dynRule idx gen cw
    -- f' :: forall rule
    --     . IAutomaton () rule
    --    => WorldIndex
    --    -> Generation
    --    -> CellWorld rule
    --    -> IO ()
    -- f' idx gen _ = do
    --   let strCode = code () (Proxy @rule)
    --   putStrLn (show idx <> ") [" <> strCode <> "], gen: " <> show gen)

--     fDyn' dynRule idx gen _ = do
--       let strCode = code dynRule (Proxy @'DynRule)
--       putStrLn (show idx <> ") [" <> strCode <> "], gen: " <> show gen)

processNewTape :: AppState -> String -> IO AppAction
processNewTape st str = do
  idx <- addTape st $ initTape str
  continueWithMsg $ "Tape idx: " <> show idx


processRun :: AppState -> RuleIndex -> TapeIndex -> IO AppAction
processRun (AppState rulesRef tapesRef) ruleIdx tapeIdx = do
  rules <- readIORef rulesRef
  tapes <- readIORef tapesRef
  case (Map.lookup ruleIdx rules, Map.lookup tapeIdx tapes) of
    (Nothing, _) -> continueWithMsg "Rule doesn't exist."
    (_, Nothing) -> continueWithMsg "Tape doesn't exist."
    (Nothing, Nothing) -> continueWithMsg "Rule and tape don't exist."
    (Just (RI proxy), Just tape1) -> do
      let eTape2 = run () proxy tape1
      case eTape2 of
        Left err -> continueWithMsg err
        Right tape2 -> do
          let tapes' = Map.insert tapeIdx tape2 tapes
          writeIORef tapesRef tapes'
          printTape' (tapeIdx, tape2)
          continue
      -- Just (DynWI dynRule gen world) -> do
      --   let world' = step dynRule world
      --   let wi = DynWI dynRule (gen + 1) world'
      --   let worlds' = Map.insert idx wi worlds
      --   writeIORef worldsRef worlds'
      --   continue

processPrintTape :: AppState -> TapeIndex -> IO AppAction
processPrintTape (AppState _ tapesRef) tapeIdx = do
  tapes <- readIORef tapesRef
  case Map.lookup tapeIdx tapes of
    Nothing -> continueWithMsg "Tape doesn't exist."
    Just tape -> do
      printTape' (tapeIdx, tape)
      continue

processLoadRule :: AppState -> String -> IO AppAction
processLoadRule appState@(AppState rulesRef _) rulePath = do
  rules <- readIORef rulesRef

  ruleStr <- readFile rulePath

  case readMaybe ruleStr of
    Nothing -> continueWithMsg "Failed to parse the rule."
    Just (rule :: R.Rule) -> do
      continueWithMsg "Parsed." -- TODO
      -- worlds <- readIORef worldsRef
      -- let idx = Map.size worlds
      -- let worlds' = Map.insert idx wi worlds
      -- writeIORef worldsRef worlds'
      -- continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)


--     Just (DynRI dynRule) -> do
--       putStrLn "\nEnter absolute world path:"
--       file <- getLine

--       eWI <- loadWorld2Dyn file dynRule

--       case eWI of
--         Left err  -> continueWithMsg ("Failed to load [" <> ruleCode <> "]: " <> err)
--         Right wi -> do
--           worlds <- readIORef worldsRef
--           let idx = Map.size worlds
--           let worlds' = Map.insert idx wi worlds
--           writeIORef worldsRef worlds'
--           continueWithMsg ("Successfully loaded [" <> ruleCode <> "], index: " <> show idx)


-- processLoadPredef :: AppState -> IO AppAction
-- processLoadPredef (AppState rulesRef worldsRef) = do
--   let predefs = [ ("gol", "./data/GoL/glider.txt")
--                 , ("seeds", "./data/Seeds/world1.txt")
--                 ]

--   putStrLn "\nPredefined worlds to load:"
--   mapM_ (\(c, f) -> putStrLn $ "[" <> c <> "]: " <> f) predefs

--   execPath <- getCurrentDirectory     -- for some reason returns the stack.yaml containing folder

--   rules <- readIORef rulesRef

--   rs <- for predefs $ \(c, f) -> do
--     case Map.lookup c rules of
--       Nothing -> pure "Unknown rule."

--       Just (RI proxy) -> do
--         let file = execPath <> "/BookSamples/CH05/ch5/" <> f
--         eWI <- loadWorld2 proxy file
--         case eWI of
--           Left err  -> pure ("Failed to load [" <> c <> "]: " <> err)
--           Right wi -> do
--             worlds <- readIORef worldsRef
--             let idx = Map.size worlds
--             let worlds' = Map.insert idx wi worlds
--             writeIORef worldsRef worlds'
--             pure ("Successfully loaded [" <> c <> "], index: " <> show idx)

--       Just (DynRI dynRule) -> do
--         let file = execPath <> "/BookSamples/CH05/ch5/" <> f
--         eWI <- loadWorld2Dyn file dynRule
--         case eWI of
--           Left err  -> pure ("Failed to load [" <> c <> "]: " <> err)
--           Right wi -> do
--             worlds <- readIORef worldsRef
--             let idx = Map.size worlds
--             let worlds' = Map.insert idx wi worlds
--             writeIORef worldsRef worlds'
--             pure ("Successfully loaded [" <> c <> "], index: " <> show idx)

--   continueWithMsg $ intercalate "\n" rs

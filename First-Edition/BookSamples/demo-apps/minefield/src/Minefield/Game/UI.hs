module Minefield.Game.UI where

import CPrelude

import System.Console.ANSI
import System.IO (hFlush, stdout)
import qualified System.IO as SIO
import qualified Data.Text as T

-- TODO: remove hardcode
-- TODO: support fields of arbitrary size
-- TODO: use ncurses or similar tools to make a directly controlled UI

resetScreen :: IO ()
resetScreen = do
  setSGR [Reset]
  clearScreen
  setCursorPosition 0 0

flushScreen :: IO ()
flushScreen = hFlush stdout

printTitle :: String -> IO ()
printTitle str = do
  setCursorPosition 1 0
  clearLine
  putStr str

printStatus :: String -> IO ()
printStatus str = do
  setCursorPosition 15 0
  clearLine
  putStr str

-- FXIME: hardcode
directions :: String
directions = "U | D | L | R | UL | UR | DL | DR"

printCommands :: [(String, Bool)] -> IO ()
printCommands cmds = do
  setCursorPosition 17 0

  putStrLn @String "Commands:"

  printSystemCommands

  forM_ cmds (\(cmd, isDir) -> do
    putStr cmd
    if isDir then (putStrLn ("   " <> directions))
             else putStrLn @String "")

  flushScreen

  where
    printSystemCommands = do
      putStrLn @String "quit | exit"
      putStrLn @String "turn"
      putStrLn @String "tick"


printDebugString :: String -> IO ()
printDebugString str = do
  setCursorPosition 16 0
  clearLine
  putStr str

withInputInvitation :: String -> IO String
withInputInvitation msg = do
  setCursorPosition 4 0
  clearFromCursorToLineEnd
  setCursorPosition 4 0
  putStr msg
  setCursorPosition 4 (length msg + 1)
  hFlush stdout
  line <- getLine
  pure $ T.unpack line

clearField :: (Int, Int) -> IO ()
clearField (w, h) = mapM_ (\h' -> do
  setCursorPosition (5 + h') 3
  clearLine
  ) [0..h+2]

fieldVPos :: Int
fieldVPos = 5
fieldHPos = 3

drawFieldFrame :: (Int, Int) -> IO ()
drawFieldFrame (w, h) = do
  setCursorPosition fieldVPos fieldHPos
  putStr @String "┌"
  putStr @String $ replicate w '─'
  putStr @String "┐"

  setCursorPosition (fieldVPos + h + 1) fieldHPos
  putStr @String "└"
  putStr @String $ replicate w '─'
  putStr @String "┘"

  mapM_ (\h' -> do
    setCursorPosition (fieldVPos + h') fieldHPos
    putStr @String "│"
    setCursorPosition (fieldVPos + h') (fieldHPos + w + 1)
    putStr @String "│") [1..h]

drawFieldObject :: (Int, Int) -> Char -> IO ()
drawFieldObject (x, y) ch = do
  setCursorPosition (fieldVPos + 1 + y) (fieldHPos + 1 + x)
  putStr @String [ch]

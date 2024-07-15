module Minefield.Game.UI where

import CPrelude

import System.Console.ANSI
import System.IO (hFlush, stdout)
import qualified System.IO as SIO


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

withInputInvitation :: String -> IO Text
withInputInvitation msg = do
  setCursorPosition 4 0
  clearFromCursorToLineEnd
  setCursorPosition 4 0
  putStr msg
  setCursorPosition 4 (length msg + 1)
  hFlush stdout
  line <- getLine
  pure line

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

module Main where

import StrongPath (Path, System, Abs, Rel, File, Dir, (</>),
  parseAbsDir, parseRelFile)
import qualified StrongPath as SP

import Data.Maybe (fromJust)

data HomeDir
type HomeAbsPath = Path System Abs (Dir HomeDir)

getAbsHomeDirPath :: IO HomeAbsPath
getAbsHomeDirPath = do
  line <- getLine
  pure (fromJust (parseAbsDir line))

data UserFile
type UserFileRelPath = Path System (Rel HomeDir) (File UserFile)

getRelUserFilePath :: IO UserFileRelPath
getRelUserFilePath = do
  line <- getLine
  pure (fromJust (parseRelFile line))

getRelFooBarFilePath :: IO (Path System (Rel HomeDir) (File UserFile))
getRelFooBarFilePath = pure . fromJust . parseRelFile $ "foo\\bar.txt"

main = do
  absHomePath <- getAbsHomeDirPath
  print absHomePath

  relUserFile <- getRelUserFilePath
  print relUserFile

  relFooBarFile <- getRelFooBarFilePath
  print relFooBarFile

  let fullUserFilePath = absHomePath </> relUserFile
  print fullUserFilePath

  -- Won't compile: two abs paths cannot be combined
  -- let invalidPath1 = absHomePath </> absHomePath
  -- print invalidPath1

  -- Won't compile: abs path cannot follow rel path
  -- let invalidPath2 = relUserFile </> absHomePath
  -- print invalidPath2

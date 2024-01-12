module Main where

import StrongPath (Path, System, Abs, Rel, File, Dir, (</>))
import qualified StrongPath as SP

import Data.Maybe (fromJust)

data HomeDir

getHomeDirPath :: IO (Path System Abs (Dir HomeDir))
getHomeDirPath = getLine >>= pure . fromJust . SP.parseAbsDir

data UserFile

getUserFilePath :: IO (Path System (Rel HomeDir) (File UserFile))
getUserFilePath = getLine >>= pure . fromJust . SP.parseRelFile

getFooBarFilePath :: IO (Path System (Rel HomeDir) (File UserFile))
getFooBarFilePath = pure . fromJust . SP.parseRelFile $ "foo\\bar.txt"

main = do
  absHomePath <- getHomeDirPath
  print absHomePath

  relUserFile <- getUserFilePath
  print relUserFile

  fooBarFile <- getFooBarFilePath
  print fooBarFile

  let fullUserFilePath = absHomePath </> relUserFile
  print fullUserFilePath

  -- Won't compile: two abs paths cannot be combined
  -- let invalidPath1 = absHomePath </> absHomePath
  -- print invalidPath1

  -- Won't compile: rel path cannot preceed abs path
  -- let invalidPath2 = relUserFile </> absHomePath
  -- print invalidPath2

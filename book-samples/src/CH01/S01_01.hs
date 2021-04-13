{-# LANGUAGE DataKinds     #-}

module CH01.S01_01 where

import Data.Vector.Indexed (Vector, ParseError (..), fromString)

type FirstName = Vector 10 Char                              -- #A

getFirstName :: IO (Either ParseError FirstName)
getFirstName = do
  putStr "Enter your first name: "
  nameStr <- getLine
  pure $ fromString nameStr                                  -- #B

greeting :: IO ()
greeting = do
  eNameVec <- getFirstName
  putStrLn $ case eNameVec of                                -- #C
    Left EmptyString     -> "Name can't be empty."
    Left OutOfBoundaries -> "Name exceeds 10 chars."
    Right nameVec        -> "Hello, " ++ show nameVec ++ "!"

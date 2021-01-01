{-# LANGUAGE DataKinds     #-}

module BookSamples.CH01.S01 where

import Data.Vector.Indexed (Vector, ParseError (..), concat, fromString, fromString')


type FirstName = Vector 10 Char
type LastName  = Vector 30 Char

data Person = Person
  { firstName :: FirstName
  , lastName  :: LastName
  } deriving Show


greeting' :: IO ()
greeting' = do
  putStrLn "Enter is your first name?"
  firstName <- getLine

  putStrLn "What is your last name?"
  lastName <- getLine

  let mbFirstNameVec = fromString' firstName
  let mbLastNameVec  = fromString' lastName

  case (mbFirstNameVec, mbLastNameVec) of
    (Nothing, _)        -> putStrLn "Sorry, your name exceeds our boundaries."
    (_, Nothing)        -> putStrLn "Sorry, your name exceeds our boundaries."
    (Just fn, Just ln)  -> do
      let person = Person fn ln
      putStrLn $ "Hello, " ++ show person ++ "!"


type Name = Vector 10 Char

getName :: IO Name
getName = do
  putStrLn "What is your name?"
  nameStr <- getLine

  case fromString nameStr of
    Left EmptyString     -> error "Name can't be empty."
    Left OutOfBoundaries -> error "Name exceeds 10 chars."
    Right nameVec        -> pure nameVec

greeting :: IO ()
greeting = do
  nameVec <- getName
  putStrLn $ "Hello, " ++ show nameVec ++ "!"

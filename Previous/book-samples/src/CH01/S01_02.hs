{-# LANGUAGE DataKinds     #-}

module CH01.S01_02 where

import Data.Vector.Indexed (Vector, ParseError (..), merge, fromString)


type FirstName = Vector 10 Char
type LastName  = Vector 20 Char

data Person = Person
  { firstName :: FirstName
  , lastName  :: LastName
  } deriving Show

getString :: String -> IO String
getString prompt = putStr prompt >> getLine

greeting :: IO ()
greeting = do
  firstName <- getString "Enter your first name: "
  lastName  <- getString "Enter your last name: "

  let eFirstNameVec = fromString firstName
  let eLastNameVec  = fromString lastName

  let ePerson = do
        firstNameVec <- eFirstNameVec
        lastNameVec  <- eLastNameVec
        pure $ Person firstNameVec lastNameVec

  let firstLastName :: Vector 30 Char = case ePerson of
        Left EmptyString     -> error "Name can't be empty."
        Left OutOfBoundaries -> error "Name exceeds 10 chars."
        Right (Person fn ln) -> merge fn ln

  putStrLn $ "Hello, " ++ show firstLastName ++ "!"

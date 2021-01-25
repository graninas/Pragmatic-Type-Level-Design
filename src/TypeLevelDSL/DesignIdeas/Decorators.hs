module Tmp.DesignIdeas.Decorators where

data ActionTag a


data Print' (str :: Symbol)
data AndThen (act1 :: ActionTag at1) (act2 :: ActionTag at2)

type Print str = MkAction (Print' str)

data PrintHelloWorld_NonDecorated (AndThen (Pirnt "Hello") (Print "World"))


data PrintHelloWorld_Decorated (NewLineDecorator (AndThen (Pirnt "Hello") (Print "World")))


type NewLineDecorator' (act :: ActionTag at)

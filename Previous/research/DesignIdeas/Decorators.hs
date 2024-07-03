module DesignIdeas.Decorators where

data IAction


data Print' (str :: Symbol)
data AndThen (act1 :: IActiont1) (act2 :: IActiont2)

type Print str = MkAction (Print' str)

data PrintHelloWorld_NonDecorated (AndThen (Pirnt "Hello") (Print "World"))


data PrintHelloWorld_Decorated (NewLineDecorator (AndThen (Pirnt "Hello") (Print "World")))


type NewLineDecorator' (act :: IActiont)

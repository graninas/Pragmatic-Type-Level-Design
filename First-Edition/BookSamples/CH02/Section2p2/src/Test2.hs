{-# LANGUAGE AllowAmbiguousTypes #-}

module Test2 where

import Control.Monad.Free
import Control.Monad


data BreadType = Baguette | Toast
  deriving (Show, Eq, Ord)

data Component
  = Bread BreadType
  | Tomato
  | Salt
  | Cheese
  deriving (Show, Eq, Ord)


data SandwichBody = SandwichBody BreadType [Component]
  deriving (Show, Eq, Ord)

data Sandwich = Sandwich BreadType (Maybe BreadType) [Component]
  deriving (Show, Eq, Ord)

data SandwichConstructor next
  = StartNewSandwich BreadType Component (SandwichBody -> next)
  | AddComponent Component SandwichBody (SandwichBody -> next)
  | FinishSandwich (Maybe BreadType) SandwichBody (Sandwich -> next)

instance Functor SandwichConstructor where
  fmap _ _ = undefined

type SandwichRecipe a = Free SandwichConstructor a


startNewSandwich :: BreadType -> Component -> SandwichRecipe SandwichBody
startNewSandwich = undefined

addComponent :: Component -> SandwichBody -> SandwichRecipe SandwichBody
addComponent = undefined

finishSandwich :: (Maybe BreadType) -> SandwichBody -> SandwichRecipe Sandwich
finishSandwich = undefined



data Crust = ThickCrust | ThinCrust
  deriving (Show, Eq, Ord)
data PizzaComponent = Salami | AmericanCheese
  deriving (Show, Eq, Ord)
data Pizza = Pizza Crust [PizzaComponent]
  deriving (Show, Eq, Ord)

data PizzaConstructor next
 = MakeCirclePizza Crust [PizzaComponent] (Pizza -> next)
 | MakeSquarePizza Crust [PizzaComponent] (Pizza -> next)


instance Functor PizzaConstructor where
  fmap _ _ = undefined

type PizzaRecipe a = Free PizzaConstructor a


makeCirclePizza :: Crust -> [PizzaComponent] -> PizzaRecipe Pizza
makeCirclePizza crust comp = liftF (MakeCirclePizza crust comp id)


data Meal
 = PreparedPizza Pizza
 | PreparedSandwich Sandwich
 deriving (Show, Eq, Ord)


data CookingMethod next
 = MakePizza (PizzaRecipe Pizza) (Pizza -> next)
 | MakeSandwich (SandwichRecipe Sandwich) (Sandwich -> next)

 | MakeRandomPizzaRecipe (PizzaRecipe Pizza -> next)





class Monad m => CSandwichRecipe m where
  -- TODO

class Monad m => CPizzaRecipe m where
  -- TODO

class Monad m => CCookingMachine m where
  cmakePizza :: CPizzaRecipe r => r Pizza -> m Pizza
  cmakeSandwitch :: CSandwichRecipe r => r Sandwich -> m Sandwich

  cmakeRandomPizzaRceipe :: CPizzaRecipe mm => m (mm Pizza)


csampleCookingMachine
  :: forall m mm
  . CPizzaRecipe mm
  => CCookingMachine m
  => m [Meal]
csampleCookingMachine = do
  -- pizza <- cmakePizza (cMakeCirclePizza ThickCrust [])
  rndPizzaRecipe :: mm Pizza <- cmakeRandomPizzaRceipe
  rndPizza <- cmakePizza rndPizzaRecipe
  pure [{-PreparedPizza pizza,-} PreparedPizza rndPizza]










makePizza :: PizzaRecipe Pizza -> CookingMachine Meal
makePizza receipe = undefined

makeSandwich :: SandwichRecipe Sandwich -> CookingMachine Meal
makeSandwich receipe = undefined

makeRandomPizzaRecipe :: CookingMachine (PizzaRecipe Pizza)
makeRandomPizzaRecipe = undefined

instance Functor CookingMethod where
  fmap _ _ = undefined

type CookingMachine a = Free CookingMethod a

myPizza :: PizzaRecipe Pizza
myPizza = undefined

mySandwich :: SandwichRecipe Sandwich
mySandwich = do
  body1 <- startNewSandwich Toast Tomato
  body2 <- addComponent Cheese body1
  body3 <- addComponent Salt body2
  finishSandwich Nothing body3

sampleCookingMachine :: CookingMachine [Meal]
sampleCookingMachine = do
  pizza <- makePizza (makeCirclePizza ThickCrust [])

  rndPizzaRecipe <- makeRandomPizzaRecipe
  rndPizza <- makePizza rndPizzaRecipe
  pure [pizza, rndPizza]



module App where

type AppMessage = String

data AppAction
  = AppFinish (Maybe AppMessage)
  | AppContinue (Maybe AppMessage)
  deriving (Show, Eq)


continue :: Applicative f => f AppAction
continue = pure (AppContinue Nothing)

continueWithMsg :: Applicative f => AppMessage -> f AppAction
continueWithMsg msg = pure (AppContinue (Just msg))

finish :: Applicative f => f AppAction
finish = pure (AppFinish Nothing)

finishWithMsg :: Applicative f => AppMessage -> f AppAction
finishWithMsg msg = pure (AppFinish (Just msg))

module Tmp.DesignIdeas.VariableADT where



data Order st = Order
    { orderId   :: Stage st 1 (Optional String) (FinStage st 2 (Mandatory String))
    , version   :: Stage st 1 (Static "1.0")    (FinStage st 2 (Static "2.0"))
    , amount    :: Money
    , currency  :: Currency
    }

type OrderTemplate = Order 1
type Order = Order 2

orderTemplate :: OrderTemplate
orderTemplate = Order
  { orderId = Nothing
  , version = static
  , amount  = 1.0f
  , currency = "USD"
  }

order :: Order
order = Order
  { orderId = "ORD-233"
  , version = static
  , amount  = 2.0f
  , currency = "USD"
  }

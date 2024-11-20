module Model.Action where

data Action 
  = LoadFinish
  | FetchStart 
  | FetchFinish 
  | CheckoutStart
  | CheckoutFinish
  | DeleteStart
  | DeleteFinish
  deriving (Eq, Ord)

instance Show Action where
  show LoadFinish     = "Application loaded"
  show FetchStart     = "Fetching changes from external git repo(s) please wait ..."
  show FetchFinish    = "Fetching is finished"
  show CheckoutStart  = "Checkouting"
  show CheckoutFinish = "Checkout is finished"
  show DeleteStart    = "Deleting"
  show DeleteFinish   = "Delete is finished"

module Model.ApplicationEvent where

data ApplicationEvent 
  = Fetch
  | Checkout
  | Delete
  deriving (Eq, Ord, Show)

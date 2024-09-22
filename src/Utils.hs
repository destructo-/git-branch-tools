module Utils 
(
  headOpiton
, getOrElse
) where


headOpiton :: [a] -> Maybe a
headOpiton []      = Nothing
headOpiton (x : _) = Just x


getOrElse :: Maybe a -> a -> a
getOrElse (Just x) _ = x
getOrElse Nothing x  = x

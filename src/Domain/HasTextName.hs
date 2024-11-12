module Domain.HasTextName where
import Data.Text

class HasTextName a where
  getName  :: a -> Text

  showName :: a -> String
  showName a = unpack $ getName a

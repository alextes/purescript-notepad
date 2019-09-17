module GenericCompare where

import Data.Foldable (length)
import Prelude ((<))

isSmallerThan :: forall a. Number -> Array a -> Boolean
isSmallerThan limit countable = length countable < limit

isSmallerThanTen :: forall a. Array a -> Boolean
isSmallerThanTen = isSmallerThan 10.0

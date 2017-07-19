module HidingPartiallyUnused where

import Data.List hiding (lookup, sort)
import GHC.OldList (sort)

foo = sort

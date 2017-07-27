module HidingPartiallyUnused where

import           Data.List   hiding (sort, unionBy)
import           GHC.OldList (sort)

foo = sort
bar = subsequences

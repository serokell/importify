-- | Extended utilities for 'Bool' type.

module Extended.Data.Bool
       ( (==>)
       ) where

import           Universum

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> x = x
infixr 4 ==>

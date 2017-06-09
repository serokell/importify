-- 
-- import Data.List (delete)
-- import Data.HashTable hiding (delete)
module TestHidingUsed where

import Data.HashTable hiding (delete)
import Data.List (delete)

func = delete

main :: IO ()
main = pure ()

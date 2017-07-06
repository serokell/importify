module HidingUsed where                -- module HidingUsed where
                                       --
import Data.HashTable hiding (delete)  -- import Data.HashTable hiding (delete)
import Data.List (delete)              -- import Data.List (delete)
                                       --
func = delete                          -- func = delete

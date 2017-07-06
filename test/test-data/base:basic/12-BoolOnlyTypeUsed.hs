-- False
-- import Data.Bool (Bool)

module BoolOnlyTypeUsed where

import Data.Bool (Bool(False))

func :: Bool
func = undefined

main :: IO ()
main = pure ()

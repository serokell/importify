-- True
-- import Data.Bool (Bool(False))

module TestThingWithPartiallyUsed where

import Data.Bool (Bool(False, True))

func :: Bool
func = False

main :: IO ()
main = pure ()

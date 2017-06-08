-- False
-- import Data.Bool (Bool)

module TestThingWithUsedType where

import Data.Bool (Bool(False))

func :: Bool
func = undefined

main :: IO ()
main = pure ()

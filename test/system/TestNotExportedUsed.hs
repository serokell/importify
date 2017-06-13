--
-- import Data.Bool (bool)
module TestNotExportedUsed where

import           Data.Bool (bool)

func = bool

main :: IO ()
main = pure ()

module BaseThingWithExample where

import           Data.Bool             (Bool (False, True))
import           Language.Haskell.Exts (DataOrNew (DataType))
import           System.IO             (putStrLn)

main :: IO ()
main = putStrLn $ "Hello!" ++ show True

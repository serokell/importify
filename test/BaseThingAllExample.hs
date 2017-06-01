module BaseThingAllExample where

import           Data.Bool             (Bool (..))
import           Data.Maybe            (Maybe (..))
import           Language.Haskell.Exts (CName (..))
import           System.IO             (putStrLn)

foo :: Bool
foo = undefined

main :: IO ()
main = putStrLn "Hello!"

-- 
-- import qualified System.IO as IO (putStrLn)

module TestVarQualifiedNamedOnlyUsed where

import qualified System.IO as IO (putStrLn)

main :: IO ()
main = IO.putStrLn "test"

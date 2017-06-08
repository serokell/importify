-- 
-- import qualified System.IO as IO (putStrLn)

module TestVarQualifiedNamedUsed where

import qualified System.IO as IO (putStrLn)

main :: IO ()
main = IO.putStrLn "test"

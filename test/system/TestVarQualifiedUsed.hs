-- 
-- import qualified System.IO (putStrLn)
module TestVarQualifiedUsed where

import qualified System.IO (putStrLn)

main :: IO ()
main = System.IO.putStrLn "test"

-- 
-- import qualified System.IO (putStrLn)
module TestVarQualifiedOnlyUsed where

import qualified System.IO (putStrLn)

main :: IO ()
main = System.IO.putStrLn "test"

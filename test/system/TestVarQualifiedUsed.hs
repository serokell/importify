-- 
-- import qualified System.IO
module TestVarQualifiedUsed where

import qualified System.IO

main :: IO ()
main = System.IO.putStrLn "test"

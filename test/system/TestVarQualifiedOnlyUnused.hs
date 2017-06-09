-- putStrLn

module TestVarQualifiedOnlyUnused where

import qualified System.IO (putStrLn)

main :: IO ()
main = pure ()

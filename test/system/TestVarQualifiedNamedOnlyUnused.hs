-- putStrLn

module TestVarQualifiedNamedOnlyUnused where

import qualified System.IO as IO (putStrLn)

main :: IO ()
main = pure ()

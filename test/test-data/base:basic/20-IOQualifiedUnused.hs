-- putStrLn

module IOQualifiedUnused where

import qualified System.IO as IO (putStrLn)

main :: IO ()
main = pure ()

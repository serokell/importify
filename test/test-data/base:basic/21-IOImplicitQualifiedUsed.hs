module IOImplicitQualifiedUsed where  -- module IOImplicitQualifiedUsed where
                                      --
import System.IO as IO (putStrLn)     -- import System.IO as IO (putStrLn)
                                      --
main :: IO ()                         -- main :: IO ()
main = putStrLn "test"                -- main = putStrLn "test"

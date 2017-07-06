module IOImplicitQualifiedUsedQualified where  -- module IOImplicitQualifiedUsedQualified where
                                               --
import System.IO as IO (putStrLn)              -- import System.IO as IO (putStrLn)
                                               --
main :: IO ()                                  -- main :: IO ()
main = IO.putStrLn "test"                      -- main = IO.putStrLn "test"

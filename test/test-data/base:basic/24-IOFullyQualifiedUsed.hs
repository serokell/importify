module IOFullyQualifiedUsed where      -- module IOFullyQualifiedUsed where
                                       --
import qualified System.IO (putStrLn)  -- import qualified System.IO (putStrLn)
                                       --
main :: IO ()                          -- main :: IO ()
main = System.IO.putStrLn "test"       -- main = System.IO.putStrLn "test"

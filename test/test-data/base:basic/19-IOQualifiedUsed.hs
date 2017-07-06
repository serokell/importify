module IOQualifiedUsed where                 -- module IOQualifiedUsed where
                                             --
import qualified System.IO as IO (putStrLn)  -- import qualified System.IO as IO (putStrLn)
                                             --
main :: IO ()                                -- main :: IO ()
main = IO.putStrLn "test"                    -- main = IO.putStrLn "test"

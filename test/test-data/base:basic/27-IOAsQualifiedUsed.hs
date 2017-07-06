module IOAsQualifiedUsed where    -- module IOAsQualifiedUsed where
                                  --
import qualified System.IO as IO  -- import qualified System.IO as IO
                                  --
main :: IO ()                     -- main :: IO ()
main = IO.putStrLn "test"         -- main = IO.putStrLn "test"

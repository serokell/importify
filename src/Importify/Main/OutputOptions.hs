-- | Contains data type which specifies output options for @importify@ commands.

module Importify.Main.OutputOptions
       ( OutputOptions (..)
       , printWithOutputOptions
       ) where

import           Universum

-- | This data type dictates how output of @importify@ should be outputed.
data OutputOptions = ToConsole        -- ^ Print to console
                   | InPlace          -- ^ Change file in-place
                   | ToFile FilePath  -- ^ Print to specified file
                   deriving (Show)

-- | Prints text using given 'OutputOptions'.
printWithOutputOptions :: OutputOptions -> Text -> FilePath -> IO ()
printWithOutputOptions ToConsole   text _    = putText text
printWithOutputOptions InPlace     text file = writeFile file text
printWithOutputOptions (ToFile to) text _    = writeFile to   text

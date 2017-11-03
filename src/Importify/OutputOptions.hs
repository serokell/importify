-- | Contains data type which specifies output options for @importify@ commands.

module Importify.OutputOptions
       ( OutputOptions (..)
       , printWithOutputOptions
       ) where

import           Universum

-- | This data type dictates how output of @importify@ should be outputed.
data OutputOptions = ToConsole        -- ^ Print to console
                   | ToFile FilePath  -- ^ Print to specified file
                   deriving (Show)

-- | Prints text using given 'OutputOptions'.
printWithOutputOptions :: OutputOptions -> Text -> IO ()
printWithOutputOptions ToConsole   text = putText text
printWithOutputOptions (ToFile to) text = writeFile to text

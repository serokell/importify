-- | Contains command to convert implicit imports to explicit.

module Importify.Main.ToExplicit
       ( importifyToExplicit
       , importifyToExplicitPath
       ) where

import           Universum

import           Path

import           Importify.Bracket
import           Importify.OutputOptions

-- | Function for @importify to-explicit@ command.
importifyToExplicit :: OutputOptions -> FilePath -> IO ()
importifyToExplicit = importifyOptionsBracket importifyToExplicitPath

-- | Return result of @importify to-explicit@ command on given file.
importifyToExplicitPath :: Path Abs File -> IO ImportifyResult
importifyToExplicitPath = importifyPathBracket (importifyAstBracket implicitToExplicitImports)

implicitToExplicitImports :: ImportifyFunction
implicitToExplicitImports = error "This is not implemented!"

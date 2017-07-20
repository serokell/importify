-- | Syntax utilities to work with modules and their names.

module Importify.Syntax.Module
       ( getModuleNameId
       , getModuleTitle
       ) where

import           Universum

import           Language.Haskell.Exts              (Module (..), ModuleName,
                                                     ModuleName (..))
import           Language.Haskell.Names.SyntaxUtils (getModuleName)

{- TODO: this function used earlier, it works, but is not used anymore
   I'll keep it in case we need it again.

-- | Returns module name of the source file.
-- We can't parse the whole file to get it because default extensions are not available yet
-- so this method uses 'NonGreedy' parsing to parse only module head.
getSourceModuleName :: Text -> String
getSourceModuleName src =
    let parseResult :: ParseResult (NonGreedy (PragmasAndModuleName SrcSpanInfo))
        parseResult = parse $ toString src
        NonGreedy (PragmasAndModuleName _ _pragmas maybeModuleName) =
            fromParseResult parseResult
        ModuleName _ modNameStr =
            fromMaybe (error "File doesn't have `module' declaration") maybeModuleName
    in modNameStr
-}

-- | Get name of module by dropping annonation.
getModuleNameId :: ModuleName l -> String
getModuleNameId (ModuleName _ id) = id

-- | Returns name of 'Module' as a 'String'.
getModuleTitle :: Module l -> String
getModuleTitle = getModuleNameId . getModuleName

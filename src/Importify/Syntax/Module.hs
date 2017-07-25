-- | Syntax utilities to work with modules and their names.

module Importify.Syntax.Module
       ( getModuleNameId
       , getModuleTitle
       , isInsideExport
       , modulePragmas
       ) where

import           Universum

import           Language.Haskell.Exts              (ExportSpec (EModuleContents),
                                                     ExportSpecList (..), Module (..),
                                                     ModuleHead (..), ModuleName,
                                                     ModuleName (..), ModulePragma)
import           Language.Haskell.Names.SyntaxUtils (dropAnn, getModuleName)

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

-- | Returns 'True' iff given 'ModuleName' is inside export list.
isInsideExport :: Maybe (ModuleHead l) -> ModuleName () -> Bool
isInsideExport moduleHead moduleName = moduleName `elem` exportedModules moduleHead

-- | Extracts list of module names exported with @module A@ way.
exportedModules :: Maybe (ModuleHead l) -> [ModuleName ()]
exportedModules moduleHead = qualifiedModuleNames
  where
    exports = do
        ModuleHead _ _ _ maybeExports <- moduleHead
        maybeExports

    qualifiedModuleNames :: [ModuleName ()]
    qualifiedModuleNames = do
      ExportSpecList  _ specs   <- maybe [] one exports
      EModuleContents _ eModule <- specs
      pure $ dropAnn eModule

-- | Extract all 'ModulePragma's from 'Module'.
modulePragmas :: Module l -> [ModulePragma l]
modulePragmas (Module _ _ pragmas _ _)            = pragmas
modulePragmas (XmlPage _ _ pragmas _ _ _ _)       = pragmas
modulePragmas (XmlHybrid _ _ pragmas _ _ _ _ _ _) = pragmas

-- | Syntax utilities to work with modules and their names.

module Importify.Syntax.Module
       ( getModuleNameId
       , getModuleTitle
       ) where

import           Universum

import qualified Data.List.NonEmpty                 as NE
import qualified Data.Text                          as T
import           Language.Haskell.Exts              (Annotated (ann),
                                                     Extension (DisableExtension),
                                                     ImportDecl (..), ImportSpecList (..),
                                                     KnownExtension (ImplicitPrelude),
                                                     Module (..), ModuleName,
                                                     ModuleName (..),
                                                     ModulePragma (LanguagePragma),
                                                     Name (Ident), SrcSpan (..),
                                                     SrcSpanInfo (..), combSpanInfo,
                                                     noSrcSpan, prettyExtension)
import           Language.Haskell.Names             (NameInfo, Scoped (..))
import           Language.Haskell.Names.SyntaxUtils (getModuleName)
import           Text.Show.Pretty                   (ppShow)

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

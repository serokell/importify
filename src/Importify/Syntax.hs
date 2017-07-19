{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Common utilities for import list processing

module Importify.Syntax
       ( InScoped
       , getImportModuleName
       , getModuleNameId
       , getModuleTitle
       , importSlice
       , pullScopedInfo
       , scopedNameInfo
       , stripEndLineComment
       , switchHidingImports
       , unscope

       , debugAST
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

-- | Short wrapper for types annotated by @Scoped SrcSpanInfo@.
-- For example, use @InScoped ImportDecl@ instead of @ImportDecl (Scoped SrcSpanInfo)@.
type InScoped t = t (Scoped SrcSpanInfo)

-- | Returns module name for 'ImportDecl' with annotation erased.
getImportModuleName :: ImportDecl l -> ModuleName ()
getImportModuleName ImportDecl{..} = () <$ importModule

-- | Keep only hiding imports making them non-hiding. This function
-- needed to collect unused hiding imports because @importTable@ doesn't
-- track information about hiding imports.
switchHidingImports :: Module SrcSpanInfo -> Module SrcSpanInfo
switchHidingImports (Module ml mhead mpragmas mimports mdecls) =
    Module ml
           mhead
           ( LanguagePragma noSrcSpan
                            [ Ident noSrcSpan
                            $ prettyExtension
                            $ DisableExtension ImplicitPrelude
                            ]
           : mpragmas)
           (mapMaybe unhide mimports)
           mdecls
  where
    unhide decl@ImportDecl{..} = importSpecs >>= \case
        ImportSpecList _ False _      -> Nothing
        ImportSpecList l True imports -> Just
                                       $ decl {importSpecs = Just
                                                           $ ImportSpecList l False imports
                                              }
switchHidingImports m = m

startAndEndLines :: SrcSpanInfo -> (Int, Int)
startAndEndLines (SrcSpanInfo SrcSpan{..} _) = (srcSpanStartLine, srcSpanEndLine)

-- | Returns pair of line numbers — first and last line of import section
-- if any import is in list.
importSlice :: [ImportDecl SrcSpanInfo] -> Maybe (Int, Int)
importSlice []               = Nothing
importSlice [ImportDecl{..}] = Just $ startAndEndLines importAnn
importSlice (x:y:xs)         = Just $ startAndEndLines
                                    $ combSpanInfo (importAnn x) (importAnn $ NE.last (y :| xs))

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

-- | Returns name of 'Module' as a 'String'.
getModuleTitle :: Module l -> String
getModuleTitle (getModuleName -> ModuleName _ name) = name

-- | Get name of module by dropping annonation.
getModuleNameId :: ModuleName l -> String
getModuleNameId (ModuleName _ id) = id

-- | Retrive 'NameInfo' from 'Scoped'.
scopedNameInfo :: Scoped l -> NameInfo l
scopedNameInfo (Scoped info _) = info

-- | Retrive 'NameInfo' from something annotated by 'Scoped'.
pullScopedInfo :: Annotated ast => ast (Scoped l) -> NameInfo l
pullScopedInfo = scopedNameInfo . ann

-- | Drop 'Scoped' annotation from 'Functor' type.
unscope :: Functor f => f (Scoped l) -> f l
unscope = fmap $ \case Scoped _ l -> l

-- | This functions strips out trailing single line comment.
stripEndLineComment :: Text -> Text
stripEndLineComment line = case T.breakOnAll "--" line of
    []               -> line
    ((stripped,_):_) -> stripped

-- | Helper function to debug different parts of AST processing.
-- TODO: remove when logging appear.
{-# WARNING debugAST "'debugAST' remains in code" #-}
debugAST :: Show a => Text -> a -> IO ()
debugAST header msg = do
    putText $ "-------------------- // " <> header <> " // --------------------"
    putText $ toText $ ppShow msg

-- | Different text utilities.

module Importify.Syntax.Text
       ( stripEndLineComment
       , debugAst
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

-- | This functions strips out trailing single line comment.
stripEndLineComment :: Text -> Text
stripEndLineComment line = case T.breakOnAll "--" line of
    []               -> line
    ((stripped,_):_) -> stripped

-- | Helper function to debug different parts of AST processing.
--
-- TODO: better utility with logging?
{-# WARNING debugAst "'debugAst' remains in code" #-}
debugAst :: Show a => Text -> a -> IO ()
debugAst header msg = do
    putText $ "-------------------- // " <> header <> " // --------------------"
    putText $ toText $ ppShow msg

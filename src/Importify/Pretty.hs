-- | This module contains functions to print imports
-- preserving original formatting.

module Importify.Pretty
       ( printLovelyImports
       ) where

import Universum

import Control.Arrow ((&&&))
import Data.Text (strip)
import Language.Haskell.Exts (Comment (..), ImportDecl (..), SrcSpan (..), SrcSpanInfo (..),
                              exactPrint, startLine)
import Language.Haskell.Exts.Syntax (Annotated (..))

import Importify.Syntax (stripEndLineComment)

import qualified Data.IntMap.Strict as IM

-- | This function takes range of origin text for import and
-- AST of imports without unused entinties and then converts
-- import declarations into list of lines that should be printed.
printLovelyImports :: Int
                   -> Int
                   -> [Comment]
                   -> [Text]
                   -> [ImportDecl SrcSpanInfo]
                   -> [Text]
printLovelyImports start end comments importsText importDecls =
    let -- map ImportDecl into (Int, [Text]) -- index of starting line to lines of text
        indexedImportLines = map (importStartLine &&& exactPrintImport comments) importDecls
        importsMap         = IM.fromList indexedImportLines

        -- find empty and single comment lines
        indexedTextLines    = zip [start..end] importsText
        emptyOrCommentLines = filter (null . strip . stripEndLineComment . snd) indexedTextLines

        -- add empty and single comment lines to result map
        importsMapWithExtraLines = foldl' (\dict (i,l) -> IM.insert i [l] dict)
                                          importsMap
                                          emptyOrCommentLines

        -- collect all values and concat them; order is guaranteed by IntMap
        resultLines = concat $ IM.elems importsMapWithExtraLines
    in resultLines

importStartLine :: ImportDecl SrcSpanInfo -> Int
importStartLine = srcSpanStartLine . srcInfoSpan . importAnn

exactPrintImport :: [Comment] -> ImportDecl SrcSpanInfo -> [Text]
exactPrintImport allComments importDecl = dropWhile null
                            $ lines
                            $ toText
                            $ importText ++ endComments
  where
    importText = exactPrint importDecl (filter commentOnLine allComments)

    -- exactPrint stops printing comments after the import statement is finished, so print those manually.
    -- This would be much easier if Language.Haskell.Exts.ExactPrint exported more stuff
    endComments = printComments
                $ filter (\(Comment _ (SrcSpan _ sl _ _ ec) _) -> ec > importEndPos && sl == curLine)
                  allComments
    printComments = intercalate " " . map ((" " ++) . printComment)
    printComment (Comment multi _ s) = if multi
                                       then "{-" ++ s ++ "-}"
                                       else "--" ++ s

    curLine = (startLine . ann) importDecl
    importEndPos = (srcSpanEndColumn . srcInfoSpan . ann) importDecl
    commentSpan (Comment _ srcSpan _) = srcSpan
    commentOnLine = (== curLine) . srcSpanStartLine . commentSpan

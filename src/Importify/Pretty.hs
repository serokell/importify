-- | This module contains functions to print imports
-- preserving original formatting.

module Importify.Pretty
       ( printLovelyImports
       ) where

import           Universum

import           Control.Arrow         ((&&&))
import qualified Data.IntMap.Strict    as IM
import           Data.Text             (strip)
import           Language.Haskell.Exts (ImportDecl (..), SrcSpanInfo (..), exactPrint,
                                        srcSpanStartLine)

-- | This function takes range of origin text for import and
-- AST of imports without unused entinties and then converts
-- import declarations into list of lines that should be printed.
printLovelyImports :: Int
                   -> Int
                   -> [Text]
                   -> [ImportDecl SrcSpanInfo]
                   -> [Text]
printLovelyImports start end importsText importDecls =
    let -- map ImportDecl into (Int, [Text]) -- index of starting line to lines of text
        indexedImportLines = map (importStartLine &&& exactPrintImport) importDecls
        importsMap         = IM.fromList indexedImportLines

        -- find indexes of empty lines
        indexedTextLines  = zip [start..end] importsText
        emptyLinesIndexes = map fst
                          $ filter (null . strip . snd) indexedTextLines

        -- add empty lines to result map
        importsMapWithEmptyLines = foldl' (\dict i -> IM.insert i [""] dict)
                                          importsMap
                                          emptyLinesIndexes

        -- collect all values and concat them; order is guaranteed by IntMap
        resultLines = concat $ IM.elems importsMapWithEmptyLines
    in resultLines

importStartLine :: ImportDecl SrcSpanInfo -> Int
importStartLine = srcSpanStartLine . srcInfoSpan . importAnn

exactPrintImport :: ImportDecl SrcSpanInfo -> [Text]
exactPrintImport importDecl = dropWhile null
                            $ lines
                            $ toText
                            $ exactPrint importDecl []

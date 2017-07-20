-- | Different text utilities.

module Importify.Syntax.Text
       ( stripEndLineComment
       , debugAst
       ) where

import           Universum

import qualified Data.Text        as T
import           Text.Show.Pretty (ppShow)

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
    putText $ unlines
            $ zipWith (\i line -> show i <> ": " <> line) [1 :: Int ..]
            $ lines
            $ toText
            $ ppShow msg

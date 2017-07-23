-- | Different text utilities.

module Importify.Syntax.Text
       ( stripEndLineComment
       , debugAST
       ) where

import           Universum

import qualified Data.Text          as T
import           Fmt                (fmt, padLeftF)
import           Text.Pretty.Simple (pShow)

-- | This functions strips out trailing single line comment.
stripEndLineComment :: Text -> Text
stripEndLineComment line = case T.breakOnAll "--" line of
    []               -> line
    ((stripped,_):_) -> stripped

-- | Helper function to debug different parts of AST processing.
--
-- TODO: better utility with logging?
{-# WARNING debugAST "'debugAst' remains in code" #-}
debugAST :: Show a => Text -> a -> IO ()
debugAST header msg = do
    let preamble = "-------------------- // " <> header <> " // --------------------\n"
    putText $ (preamble <>)
            $ unlines
            $ zipWith (\i line -> lineNumber i <> ": " <> line) [1..]
            $ lines
            $ toText
            $ pShow msg
  where
    lineNumber :: Int -> Text
    lineNumber = fmt . padLeftF 4 ' '  -- padding 4 should be enough (no bigger 9999)

-- | This module contains data type for parsing exception and pretty
-- formatting of such exceptions.

module Importify.ParseException
       ( ModuleParseException (..)
       , prettyParseResult
       , reportErrorsIfAny
       ) where

import           Universum

import           Fmt                   (Builder, blockListF, build, fmt, fmtLn, indent,
                                        listF, ( #| ), (|#))
import           Language.Haskell.Exts (ParseResult (..), prettyPrint, SrcLoc)

import           Extended.Data.Str     (charWrap, wordWrap)
import           Extended.System.Wlog  (printWarning)

data ModuleParseException = MPE SrcLoc !String
    deriving (Show)

instance Exception ModuleParseException
instance Buildable ModuleParseException where
    build (MPE loc reason) = "Location:\n"
                          #| indent 4 (build $ charWrap 80 $ prettyPrint loc)
                          |# "Reason:\n"
                          #| indent 4 (build $ wordWrap 80 reason)
                          |# ""

-- | Converts 'ParseResult' into 'Either' making error look pretty.
prettyParseResult :: ParseResult (ast, comments)
                  -> Either ModuleParseException ast
prettyParseResult (ParseOk (moduleAst, _)) = Right moduleAst
prettyParseResult (ParseFailed loc reason) = Left $ MPE loc reason

-- | Pretty printing 'NonEmpty' list of errors in really nice way.
prettyParseErrors :: Text -> NonEmpty ModuleParseException -> Text
prettyParseErrors libName exceptions =
    "Next errors occured during caching of package: "#|libName|#"\n"
 #| indent 4 (blockListF exceptions) |# ""

-- | Prints parse errors if list of errors is not empty.
reportErrorsIfAny :: [ModuleParseException] -> Text -> IO ()
reportErrorsIfAny exceptions libName = whenNotNull exceptions $
    printWarning . prettyParseErrors libName

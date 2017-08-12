-- | This module contains data type for parsing exception and pretty
-- formatting of such exceptions.

module Importify.ParseException
       ( ModuleParseException (..)
       , eitherParseResult
       , reportErrorsIfAny
       , setMpeFile
       ) where

import           Universum

import           Fmt                   (Builder, blockListF, build, fmt, fmtLn, indent,
                                        listF, ( #| ), (|#))
import           Language.Haskell.Exts (ParseResult (..), SrcLoc (srcFilename),
                                        prettyPrint)

import           Extended.Data.Str     (charWrap, wordWrap)
import           Extended.System.Wlog  (printWarning)

data ModuleParseException = MPE !SrcLoc !String
    deriving (Show)

instance Exception ModuleParseException
instance Buildable ModuleParseException where
    build (MPE loc reason) = "Location:\n"
                          #| indent 4 (build $ charWrap 80 $ prettyPrint loc)
                          |# "Reason:\n"
                          #| indent 4 (build $ wordWrap 80 reason)
                          |# ""

-- | Updates file name of error location. Sometimes error location
-- looks like @- Location: : -1: -1@ which is not very helpful.
setMpeFile :: FilePath -> ModuleParseException -> ModuleParseException
setMpeFile modulePath (MPE loc msg) = MPE (loc {srcFilename = modulePath}) msg

-- | Converts 'ParseResult' into 'Either' making error look pretty.
eitherParseResult :: ParseResult res
                  -> Either ModuleParseException res
eitherParseResult (ParseOk res)            = Right res
eitherParseResult (ParseFailed loc reason) = Left $ MPE loc reason

-- | Pretty printing 'NonEmpty' list of errors in really nice way.
prettyParseErrors :: Text -> NonEmpty ModuleParseException -> Text
prettyParseErrors libName exceptions =
    "Next errors occured during caching of package: "#|libName|#"\n"
 #| indent 4 (blockListF exceptions) |# ""

-- | Prints parse errors if list of errors is not empty.
reportErrorsIfAny :: [ModuleParseException] -> Text -> IO ()
reportErrorsIfAny exceptions libName = whenNotNull exceptions $
    printWarning . prettyParseErrors libName

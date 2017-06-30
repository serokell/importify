{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains utilities for dealing with
-- @-XCPP@ extension in files.

module Importify.CPP
       ( parseModuleFile
       , parseWithCPP
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Fmt                       (( #| ), (|#))
import           Language.Haskell.Exts     (Comment, Extension, Module, ParseMode (..),
                                            ParseResult (..), SrcSpanInfo,
                                            defaultParseMode, prettyPrint)
import qualified Language.Haskell.Exts     as LHE (SrcLoc)
import           Language.Haskell.Exts.CPP (defaultCpphsOptions,
                                            parseFileWithCommentsAndCPP)
import           Path                      (Abs, File, Path, fromAbsFile)

data ModuleParseException = MPE !LHE.SrcLoc !String
    deriving (Show)

instance Exception ModuleParseException
instance Buildable ModuleParseException where
    build (MPE loc reason) = ""#|prettyPrint loc|#" because of: "#|reason|#""

-- | Parses file at given path without lines of c-preprocessor.
-- Some additional handling is required because @haskell-src-exts@ can't
-- handle @-XCPP@.
parseWithCPP :: Path Abs File -> [Extension] -> IO $ ParseResult $ (Module SrcSpanInfo, [Comment])
parseWithCPP pathToModule cabalExtensions = do
    let moduleFile = fromAbsFile pathToModule
    parseFileWithCommentsAndCPP defaultCpphsOptions
                                (defaultParseMode { extensions = cabalExtensions })
                                moduleFile

-- | Parse 'Module' by given 'Path' with given 'Extension's converting
-- parser errors into human readable text.
parseModuleFile :: [Extension] -> Path Abs File -> IO $ Either Text $ Module SrcSpanInfo
parseModuleFile cabalExtensions pathToModule =
    parseWithCPP pathToModule cabalExtensions >>= \case
        ParseOk (moduleAST, _) -> return $ Right moduleAST
        ParseFailed loc reason -> return $ Left
                                         $ "Ignoring module cache := "#|MPE loc reason|#""

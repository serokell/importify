{-# LANGUAGE TypeOperators #-}

-- | This module contains utilities for dealing with
-- @-XCPP@ extension in files.

module Importify.CPP
       ( parseModuleFile
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Fmt                       (( #| ), (|#))
import           Language.Haskell.Exts     (Extension, Module, ParseMode (..),
                                            ParseResult (..), SrcSpanInfo,
                                            defaultParseMode, prettyPrint)
import qualified Language.Haskell.Exts     as LHE (SrcLoc)
import           Language.Haskell.Exts.CPP (defaultCpphsOptions, CpphsOptions (includes),
                                            parseFileWithCommentsAndCPP)
import           Path                      (Abs, File, Path, fromAbsFile)

data ModuleParseException = MPE !LHE.SrcLoc !String
    deriving (Show)

instance Exception ModuleParseException
instance Buildable ModuleParseException where
    build (MPE loc reason) = ""#|prettyPrint loc|#" because of: "#|reason|#""

-- | Parse 'Module' by given 'Path' with given 'Extension's converting
-- parser errors into human readable text. Some additional handling is
-- required because @haskell-src-exts@ can't handle @-XCPP@.
parseModuleFile :: [Extension]    -- ^ List of extensions from .cabal file
                -> [FilePath]     -- ^ Filenames of .h files to include
                -> Path Abs File  -- ^ Path to module
                -> IO $ Either Text $ Module SrcSpanInfo
parseModuleFile cabalExtensions includeFiles pathToModule = do
    parseResult <- parseFileWithCommentsAndCPP (defaultCpphsOptions {includes = includeFiles})
                                               (defaultParseMode {extensions = cabalExtensions})
                                               (fromAbsFile pathToModule)
    case parseResult of
        ParseOk (moduleAST, _) -> return $ Right moduleAST
        ParseFailed loc reason -> return $ Left
                                         $ "Ignoring module cache := "#|MPE loc reason|#""

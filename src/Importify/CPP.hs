{-# LANGUAGE TypeOperators #-}

-- | This module contains functions for parsing /Haskell/ modules also
-- dealing with @-XCPP@ extension in files.

module Importify.CPP
       ( parseModuleFile
       ) where

import           Universum

import           Fmt                       (( #| ), (|#))
import           Language.Haskell.Exts     (Extension, Module, ParseMode (..),
                                            SrcSpanInfo, defaultParseMode)
import           Language.Haskell.Exts.CPP (CpphsOptions (includes), defaultCpphsOptions,
                                            parseFileWithCommentsAndCPP)
import           Path                      (Abs, File, Path, fromAbsFile)

import           Importify.ParseException  (ModuleParseException, prettyParseResult)

-- | Parse 'Module' by given 'Path' with given 'Extension's converting
-- parser errors into human readable text. Some additional handling is
-- required because @haskell-src-exts@ can't handle @-XCPP@.
parseModuleFile :: [Extension]    -- ^ List of extensions from .cabal file
                -> [FilePath]     -- ^ Filenames of .h files to include
                -> Path Abs File  -- ^ Path to module
                -> IO $ Either ModuleParseException $ Module SrcSpanInfo
parseModuleFile cabalExtensions includeFiles pathToModule =
    fmap prettyParseResult $
         parseFileWithCommentsAndCPP (defaultCpphsOptions {includes = includeFiles})
                                     (defaultParseMode {extensions = cabalExtensions})
                                     (fromAbsFile pathToModule)

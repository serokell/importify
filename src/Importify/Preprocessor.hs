{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | This module contains functions for parsing /Haskell/ modules also
-- dealing with @-XCPP@ extension in files.

module Importify.Preprocessor
       ( parseModuleWithPreprocessor
       ) where

import           Universum

import qualified Autoexporter              (mainWithArgs)
import           Language.Haskell.Exts     (Extension, Module,
                                            ModulePragma (OptionsPragma), ParseMode (..),
                                            SrcSpanInfo, Tool (GHC), defaultParseMode)
import           Language.Haskell.Exts.CPP (CpphsOptions (includes), defaultCpphsOptions,
                                            parseFileWithCommentsAndCPP)
import           Path                      (Abs, File, Path, fromAbsFile, fromRelFile,
                                            parseRelFile, (-<.>))
import           System.Directory          (removeFile)

import           Importify.ParseException  (ModuleParseException, prettyParseResult)
import           Importify.Paths           (getCurrentPath)
import           Importify.Syntax          (debugAST, getModuleTitle, modulePragmas)

-- | Parse module after preproccessing this module with possibly
-- custom preprocessor. It first calls parsing with CPP, then reads
-- @OPTIONS_GHC@ to handle custom preprocessors. Now only @autoexporter@
-- supported among all custom preprocessors.
parseModuleWithPreprocessor
    :: [Extension]    -- ^ List of extensions from .cabal file
    -> [FilePath]     -- ^ Filenames of .h files to include
    -> Path Abs File  -- ^ Path to module
    -> IO $ Either ModuleParseException $ Module SrcSpanInfo
parseModuleWithPreprocessor extensions includeFiles pathToModule =
    parseModuleAfterCPP extensions includeFiles pathToModule >>= \case
      err@(Left _exception)    -> return err
      mdl@(Right parsedModule) -> case autoexportedArgs parsedModule of
        Nothing       -> return mdl
        Just autoArgs -> do
          let modulePath       = fromAbsFile pathToModule
          outputFilePath      <- pathToModule -<.> ".auto"
          let outputFileName   = fromAbsFile outputFilePath
          let preprocessorArgs = [modulePath, modulePath, outputFileName]
          Autoexporter.mainWithArgs (preprocessorArgs ++ autoArgs)
          parseModuleAfterCPP extensions includeFiles outputFilePath
            <* removeFile outputFileName

-- | Parse 'Module' by given 'Path' with given 'Extension's converting
-- parser errors into human readable text. Some additional handling is
-- required because @haskell-src-exts@ can't handle @-XCPP@.
parseModuleAfterCPP :: [Extension]    -- ^ List of extensions from .cabal file
                    -> [FilePath]     -- ^ Filenames of .h files to include
                    -> Path Abs File  -- ^ Path to module
                    -> IO $ Either ModuleParseException $ Module SrcSpanInfo
parseModuleAfterCPP cabalExtensions includeFiles pathToModule =
    fmap prettyParseResult $
         parseFileWithCommentsAndCPP (defaultCpphsOptions {includes = includeFiles})
                                     (defaultParseMode {extensions = cabalExtensions})
                                     (fromAbsFile pathToModule)

autoexportedArgs :: forall l. Module l -> Maybe [String]
autoexportedArgs = head . mapMaybe autoexporterPragma . modulePragmas
  where
    autoexporterPragma :: ModulePragma l -> Maybe [String]
    autoexporterPragma pragma = do
      OptionsPragma _ tool args <- Just pragma
      GHC <- tool
      "-F":"-pgmF":"autoexporter":autoArgs <- Just $ words $ toText args
      pure $ map toString autoArgs

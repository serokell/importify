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
                                            ModulePragma (OptionsPragma),
                                            ParseMode (extensions), SrcSpanInfo,
                                            Tool (GHC), defaultParseMode, noLoc)
import           Language.Haskell.Exts.CPP (CpphsOptions (includes), defaultCpphsOptions,
                                            parseFileWithCommentsAndCPP)
import           Path                      (Abs, File, Path, fromAbsFile, (-<.>))
import           Path.IO                   (removeFile)

import           Importify.ParseException  (ModuleParseException (MPE), eitherParseResult)
import           Importify.Syntax          (modulePragmas)

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
    (join $ errorForcer <$> parseModuleAfterCPP extensions includeFiles pathToModule)
      `catch`
    (fmap Left . cppHandler) >>= \case
      err@(Left _exception)    -> return err
      mdl@(Right parsedModule) -> case autoexportedArgs parsedModule of
        Nothing       -> return mdl
        Just autoArgs -> do
          let modulePath       = fromAbsFile pathToModule
          outputFilePath      <- pathToModule -<.> ".auto"
          let preprocessorArgs = [modulePath, modulePath, fromAbsFile outputFilePath]
          Autoexporter.mainWithArgs (preprocessorArgs ++ autoArgs)
          parseModuleAfterCPP extensions includeFiles outputFilePath
            <* removeFile outputFilePath
  where
    -- This forcer is used because without it @cppHandler@ below doesn't catch exception.
    errorForcer res = evaluateWHNF (show res :: String) >> return res
    {- [IMRF-91]: This exception handler was introduced because
                  of error in filelock-0.1.0.1 package:

       importify: #error No backend is available
       in /home/fenx/programming/haskell/serokell/importify/.importify
          /filelock-0.1.0.1/System/FileLock.hs  at line 44 col 1
       CallStack (from HasCallStack):
         error, called at ./Language/Preprocessor/Cpphs/CppIfdef.hs:113:21 in
         cpphs-1.20.8-87uHpRVbMaP4k1m97GGc18:Language.Preprocessor.Cpphs.CppIfdef
    -}
    cppHandler :: SomeException -> IO ModuleParseException
    cppHandler = return . MPE noLoc . show

-- | Parse 'Module' by given 'Path' with given 'Extension's converting
-- parser errors into human readable text. Some additional handling is
-- required because @haskell-src-exts@ can't handle @-XCPP@.
parseModuleAfterCPP :: [Extension]    -- ^ List of extensions from .cabal file
                    -> [FilePath]     -- ^ Filenames of .h files to include
                    -> Path Abs File  -- ^ Path to module
                    -> IO $ Either ModuleParseException $ Module SrcSpanInfo
parseModuleAfterCPP cabalExtensions includeFiles pathToModule =
     second fst . eitherParseResult
 <$> parseFileWithCommentsAndCPP (defaultCpphsOptions {includes = includeFiles})
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

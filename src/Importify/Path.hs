{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE QuasiQuotes    #-}
{-# LANGUAGE TypeOperators  #-}

-- | This module contains common utilities for working with importify cache.

module Importify.Path
       ( -- * Predefined directories
         importifyDir
       , importifyPath
       , extensionsFile
       , extensionsPath
       , modulesFile
       , modulesPath
       , symbolsDir
       , symbolsPath

         -- * Paths for tests
       , testDataPath
       , testUnusedPath

         -- * Utility functions to work with files and directories
       , decodeFileOrMempty
       , doInsideDir
       , findCabalFile
       , lookupToRoot
       ) where

import           Universum

import           Data.Aeson           (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString      as BS (readFile)
import           Fmt                  ((+|), (+||), (|+), (||+))
import           Path                 (Abs, Dir, File, Path, Rel, dirname, fromAbsDir,
                                       fromAbsFile, fromRelDir, fromRelFile, parent,
                                       reldir, relfile, toFilePath, (</>))
import           Path.IO              (doesFileExist, ensureDir, getCurrentDir, listDir)
import           System.FilePath      (takeExtension)
import           Turtle               (cd, pwd)

import           Extended.System.Wlog (printNotice, printWarning)

importifyPath :: Path Rel Dir
importifyPath = [reldir|.importify/|]

-- | Path to file that stores mapping from module names to their packages.
modulesPath :: Path Rel File
modulesPath = [relfile|modules|]

symbolsPath :: Path Rel Dir
symbolsPath = [reldir|symbols/|]

-- | Path to golden tests.
testDataPath :: Path Rel Dir
testDataPath = [reldir|test/test-data/|]

-- | Path to tests for removing unused imports.
testUnusedPath :: Path Rel Dir
testUnusedPath = testDataPath </> [reldir|unused|]

-- | Path to JSON-encoded Map from target to its list of default extensions.
extensionsPath :: Path Rel File
extensionsPath = [relfile|extensions|]

importifyDir, extensionsFile, modulesFile, symbolsDir :: FilePath
importifyDir   = fromRelDir  importifyPath
extensionsFile = fromRelFile extensionsPath
modulesFile    = fromRelFile modulesPath
symbolsDir     = fromRelDir  symbolsPath

-- | Returns relative path to cabal file under given directory.
findCabalFile :: MonadIO m => Path Abs Dir -> m $ Maybe $ Path Abs File
findCabalFile projectPath = do
    (_, projectFiles) <- listDir projectPath
    return $ find isCabalFile projectFiles

isCabalFile :: Path Abs File -> Bool
isCabalFile = (== ".cabal") . takeExtension . fromAbsFile

-- | Create given directory and perform given action inside it.
doInsideDir :: (MonadIO m, MonadMask m) => Path Abs Dir -> m a -> m a
doInsideDir dir action = do
    thisDirectory <- pwd
    bracket_ (do ensureDir dir
                 cd $ fromString $ fromAbsDir dir)
             (cd thisDirectory)
             action

-- | Walk up till root while unpure predicate is 'False'. Returns
-- absolute path to directory where predicate is 'True' and suffix of
-- current directory prepended to given file.
lookupToRoot :: (Path Abs Dir -> IO Bool)
             -> Path Rel File
             -> IO (Maybe (Path Abs Dir, Path Rel File))
lookupToRoot predicate relativeFile = do
    currentDir <- getCurrentDir
    pathLoop currentDir relativeFile
  where
    pathLoop :: Path Abs Dir -> Path Rel File -> IO (Maybe (Path Abs Dir, Path Rel File))
    pathLoop directory file = do
        predicateIsTrue <- predicate directory
        if predicateIsTrue then
            return $ Just (directory, file)
        else if parent directory == directory then  -- fixpoint reached
            return Nothing
        else do
            let parentDir = parent  directory
            let   thisDir = dirname directory
            pathLoop parentDir (thisDir </> file)

-- | Tries to read file and then 'decode' it. If either of two phases
-- fails then 'mempty' returned and warning is printed to console.
decodeFileOrMempty :: forall t m f b .
                      (FromJSON t, Monoid m, MonadIO f)
                   => Path b File  -- ^ Path to json data
                   -> (t -> f m)   -- ^ Action from decoded value
                   -> f m
decodeFileOrMempty file onDecodedContent = do
    isFileExist <- doesFileExist file

    if isFileExist then
        eitherDecodeStrict <$> liftIO (BS.readFile $ toFilePath file) >>= \case
            Right value -> onDecodedContent value
            Left msg    -> do
              let warning = "File '"+||file||+"' decoded incorrectly because of: "+|msg|+""
              mempty <$ printWarning warning
    else do
        let msg = "File '"+||file||+
                  "' doesn't exist: caching first time or previous caching failed"
        mempty <$ printNotice msg

{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

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
       , testDataPath
       , testDataDir

         -- * Utility functions to work with files and directories
       , decodeFileOrMempty
       , doInsideDir
       , findCabalFile
       ) where

import           Universum

import           Data.Aeson           (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString      as BS (readFile)
import           Fmt                  ((+|), (+||), (|+), (||+))
import           Path                 (Abs, Dir, File, Path, Rel, fromAbsDir, fromAbsFile,
                                       fromRelDir, fromRelFile, reldir, relfile,
                                       toFilePath)
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

-- | Path to JSON-encoded Map from target to its list of default extensions.
extensionsPath :: Path Rel File
extensionsPath = [relfile|extensions|]

importifyDir, extensionsFile, modulesFile, symbolsDir, testDataDir :: FilePath
importifyDir   = fromRelDir  importifyPath
extensionsFile = fromRelFile extensionsPath
modulesFile    = fromRelFile modulesPath
symbolsDir     = fromRelDir  symbolsPath
testDataDir    = fromRelDir  testDataPath

-- | Returns relative path to cabal file under given directory.
findCabalFile :: MonadIO m => Path Abs Dir -> m $ Maybe $ Path Abs File
findCabalFile projectPath = do
    (_, projectFiles) <- listDir projectPath
    return $ find isCabalFile projectFiles

isCabalFile :: Path Abs File -> Bool
isCabalFile = (== ".cabal") . takeExtension . fromAbsFile

-- | Create given directory and perform given action inside it.
doInsideDir :: (MonadIO m, MonadMask m) => Path Abs Dir -> m () -> m ()
doInsideDir dir action = do
    thisDirectory <- pwd
    bracket_ (do ensureDir dir
                 cd $ fromString $ fromAbsDir dir)
             (cd thisDirectory)
             action

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

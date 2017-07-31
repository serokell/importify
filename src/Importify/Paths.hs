{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains common utilities for working with importify cache.

module Importify.Paths
       ( -- * Predefined directories
         cacheDir
       , cachePath
       , extensionsFile
       , extensionsPath
       , modulesFile
       , modulesPath
       , symbolsDir
       , symbolsPath
       , targetsFile
       , targetsPath
       , testDataPath
       , testDataDir

         -- * Utility functions to work with files and directories
       , decodeFileOrMempty
       , doInsideDir
       , findCabalFile
       , getCurrentPath
       ) where

import           Universum

import           Data.Aeson           (FromJSON, eitherDecodeStrict)
import qualified Data.ByteString      as BS (readFile)
import           Path                 (Abs, Dir, File, Rel, fromAbsDir, fromRelDir,
                                       fromRelFile, parseAbsDir, parseRelFile, reldir,
                                       relfile, (</>))
import           Path.Internal        (Path (..))
import           System.Directory     (createDirectoryIfMissing, doesFileExist,
                                       getCurrentDirectory, listDirectory)
import           System.FilePath      (takeExtension)
import           Turtle               (cd, pwd)

import           Extended.System.Wlog (printNotice, printWarning)

cachePath :: Path Rel Dir
cachePath = [reldir|.importify/|]

-- | Path to file that stores mapping from module names to their packages.
modulesPath :: Path Rel File
modulesPath = [relfile|modules|]

symbolsPath :: Path Rel Dir
symbolsPath = [reldir|symbols/|]

-- | Path to JSON-encoded Map from project module name to
-- its target (i.e. __library__, __executable__).
targetsPath :: Path Rel File
targetsPath = [relfile|targets|]

-- | Path to golden tests.
testDataPath :: Path Rel Dir
testDataPath = [reldir|test/test-data/|]

-- | Path to JSON-encoded Map from target to its list of default extensions.
extensionsPath :: Path Rel File
extensionsPath = [relfile|extensions|]

cacheDir, extensionsFile, modulesFile, symbolsDir, targetsFile, testDataDir :: FilePath
cacheDir       = fromRelDir  cachePath
extensionsFile = fromRelFile extensionsPath
modulesFile    = fromRelFile modulesPath
symbolsDir     = fromRelDir  symbolsPath
targetsFile    = fromRelFile targetsPath
testDataDir    = fromRelDir  testDataPath

-- | Get absolute path to given directory.
getCurrentPath :: IO (Path Abs Dir)
getCurrentPath = do
    thisDirectory <- getCurrentDirectory
    parseAbsDir thisDirectory

-- | Returns relative path to cabal file under given directory.
findCabalFile :: Path Abs Dir -> IO $ Maybe $ Path Abs File
findCabalFile projectPath = do
    projectDirectoryContent <- listDirectory $ fromAbsDir projectPath
    let cabalFiles           = filter ((== ".cabal") . takeExtension)
                                      projectDirectoryContent
    cabalFilePath <- traverse parseRelFile $ head cabalFiles
    return $ fmap (projectPath </>) cabalFilePath

createCacheDir :: Path Abs Dir -> IO ()
createCacheDir importifyPath = do
    let importifyDir = fromAbsDir importifyPath
    createDirectoryIfMissing True importifyDir -- creates ./.importify

-- | Create given directory and perform given action inside it.
doInsideDir :: Path Abs Dir -> IO () -> IO ()
doInsideDir dir action = do
    thisDirectory <- pwd
    bracket_ (do createCacheDir dir
                 cd $ fromString $ fromAbsDir dir)
             (cd thisDirectory)
             action

-- | Tries to read file and then 'decode' it. If either of two phases
-- fails then 'mempty' returned and warning is printed to console.
decodeFileOrMempty :: (FromJSON t, Monoid m) => FilePath -> (t -> IO m) -> IO m
decodeFileOrMempty file onDecodedContent = do
    let textFile = toText file

    isFileExist <- doesFileExist file
    if isFileExist then
        eitherDecodeStrict <$> BS.readFile file >>= \case
            Left msg -> do
                printWarning $ textFile
                            <> " decoded incorrectly because of: "
                            <> toText msg
                return mempty
            Right value -> onDecodedContent value
    else do
        let msg = textFile
               <> " doesn't exist: "
               <> "caching first time or previous caching failed"
        printNotice msg
        return mempty

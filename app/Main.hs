{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @baseEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main where

import           Universum

import           Data.Aeson             (decode, encode)
import           Language.Haskell.Exts  (Extension, Module (..), fromParseResult,
                                         parseExtension, parseFileContentsWithExts,
                                         prettyPrint)
import           Language.Haskell.Names (annotate, loadBase)
import           Turtle                 (cd, pwd, shell)

import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as Map
import           Importify.Cabal        (ExtensionsMap, TargetMap, getExtensionMaps,
                                         getLibs, moduleNameToPath, readCabal)
import           Importify.Cache        (cachePath)
import           Importify.Common       (getModuleName, importSlice)
import           Importify.Resolution   (collectUnusedSymbols)
import           Importify.Tree         (removeIdentifiers)
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesFileExist)

import           Options                (CabalCacheOptions (..), Command (..),
                                         SingleFileOptions (..), parseOptions)

main :: IO ()
main = do
    opts <- parseOptions
    case opts of
        SingleFile sfOpts -> importifySingleFile sfOpts
        CabalCache ccOpts -> buildCabalCache ccOpts

importifySingleFile :: SingleFileOptions -> IO ()
importifySingleFile SingleFileOptions{..} = do
    fileContent <- readFile sfoFilename
    let moduleName = getModuleName fileContent
    extensionMaps <- readExtensionMaps
    let exts = fromMaybe [] $ getExtensions moduleName extensionMaps
    let ast@(Module _ _ _ imports _) = fromParseResult $ parseFileContentsWithExts exts
                                                       $ toString fileContent

    whenJust (importSlice imports) $ \(start, end) -> do
        let codeLines        = lines fileContent
        let (preamble, rest) = splitAt (start - 1) codeLines
        let (_, decls)       = splitAt (end - start + 1) rest

        baseEnvironment <- loadBase
        let annotatedAST = annotate baseEnvironment ast
        let annotations  = toList annotatedAST
        let unusedIds    = collectUnusedSymbols baseEnvironment imports annotations

        let newImports = removeIdentifiers unusedIds imports

        putText $ unlines preamble
               <> toText (unlines $ map (toText . prettyPrint) newImports)
               <> unlines decls

getExtensions :: String -> Maybe (TargetMap, ExtensionsMap) -> Maybe [Extension]
getExtensions moduleName maps = do
    (targetMap, extensionsMap) <- maps
    let modulePath = moduleNameToPath moduleName
    target <- Map.lookup modulePath targetMap
    extensions <- Map.lookup target extensionsMap
    pure $ map parseExtension extensions

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} = do
    cabalDesc <- readCabal ccoFilename
    unlessM (doesDirectoryExist cachePath) $ createDirectory cachePath
    cd (fromString cachePath)

    -- Extension maps
    let (targetMaps, extensionMaps) = getExtensionMaps cabalDesc
    BS.writeFile targetsMapFilename $ encode targetMaps
    BS.writeFile extensionsMapFilename $ encode extensionMaps

    -- Libraries
    let libs = getLibs cabalDesc
    print libs
    -- move to cache directory and download-unpack all libs there
    forM_ libs $ \libName -> () <$ shell ("stack unpack " <> toText libName) empty

    cd ".."

readExtensionMaps :: IO (Maybe (TargetMap, ExtensionsMap))
readExtensionMaps = do
    cd (fromString cachePath)
    targetsExist <- doesFileExist targetsMapFilename
    extensionsExist <- doesFileExist extensionsMapFilename
    if not (targetsExist && extensionsExist) then do
        cd ".."
        pure Nothing
    else do
        targetsFile <- BS.readFile targetsMapFilename
        extensionsFile <- BS.readFile extensionsMapFilename
        cd ".."
        pure $ fmap tup (decode targetsFile) <*> (decode extensionsFile)
  where
    tup :: a -> b -> (a, b)
    tup a b = (a, b)

targetsMapFilename :: String
targetsMapFilename = "targets"

extensionsMapFilename :: String
extensionsMapFilename = "extensions"

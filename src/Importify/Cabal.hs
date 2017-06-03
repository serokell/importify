-- | This module contains common utilities for parsing .cabal files
-- and manipulating it's AST.

module Importify.Cabal
       ( getLibs
       , getExtensionMaps
       , readCabal
       , moduleNameToPath
       , TargetMap
       , ExtensionsMap
       ) where

import           Universum                             hiding (fromString)

import           Data.List                             (nub)
import qualified Data.Map.Strict                       as Map
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (BuildInfo, Executable,
                                                        GenericPackageDescription (..),
                                                        Library, buildInfo,
                                                        condExecutables, condTreeData,
                                                        defaultExtensions, exeModules,
                                                        libBuildInfo, libModules,
                                                        modulePath, targetBuildDepends)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)
import           Language.Haskell.Extension            (Extension (..))

type TargetMap = Map String String
type ExtensionsMap = Map String [String]

readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription normal

-- TODO: what about version bounds?
getLibs :: GenericPackageDescription -> [String]
getLibs = nub . concat . map (map dependencyName . targetBuildDepends) . getBuildInfos

getBuildInfos :: GenericPackageDescription -> [BuildInfo]
getBuildInfos GenericPackageDescription{..} =
    maybe [] ((:[]) . libBuildInfo . condTreeData) condLibrary ++
    map (buildInfo . condTreeData . snd) condExecutables

getExtensionMaps :: GenericPackageDescription -> (TargetMap, ExtensionsMap)
getExtensionMaps GenericPackageDescription{..} =
    ( foldr Map.union Map.empty $ libTargetsMaps ++ exeTargetsMaps
    , foldr Map.union Map.empty $ libExtensionsMaps ++ exeExtensionsMaps)
  where
    (libTargetsMaps, libExtensionsMaps) = unzip $ map (collectLibraryMaps . condTreeData) $ maybeToList condLibrary
    (exeTargetsMaps, exeExtensionsMaps) = unzip $ do
        (name, condTree) <- condExecutables
        pure $ collectExecutableMaps name $ condTreeData condTree

collectLibraryMaps :: Library -> (TargetMap, ExtensionsMap)
collectLibraryMaps lib = collectModuleMaps "library" (map toFilePath $ libModules lib) (defaultExtensions $ libBuildInfo lib)

collectExecutableMaps :: String -> Executable -> (TargetMap, ExtensionsMap)
collectExecutableMaps exeName exe = collectModuleMaps ("executable " ++ exeName) (exePath:(map toFilePath $ exeModules exe)) (defaultExtensions $ buildInfo exe)
  where
    exePath = take ((length $ modulePath exe) - 3) $ modulePath exe

collectModuleMaps :: String -> [String] -> [Extension] -> (TargetMap, ExtensionsMap)
collectModuleMaps target mods exts =
    ( Map.fromList $ zip mods (repeat target)
    , Map.singleton target (map showExt exts)
    )

dependencyName :: Dependency -> String
dependencyName (Dependency PackageName{..} _) = unPackageName

showExt :: Extension -> String
showExt (EnableExtension ext)   = show ext
showExt (DisableExtension ext)  = "No" ++ show ext
showExt (UnknownExtension name) = error "Unknown extension: " ++ name

moduleNameToPath :: String -> String
moduleNameToPath modNameStr = toFilePath modName
  where
    modName :: ModuleName
    modName = fromString modNameStr

-- This function works but isn't used anywhere
{-
findModuleBuildInfo :: String -> GenericPackageDescription -> Maybe BuildInfo
findModuleBuildInfo modNameStr pkg@GenericPackageDescription{..} =
    lookupExecutable <|> lookupExposedModules <|> lookupOtherModules
  where
    modName = fromString modNameStr
    lookupExecutable = asum $ do
        (_name, condExecutable) <- condExecutables
        let exec = condTreeData condExecutable
        if (modulePath exec) == (toFilePath modName ++ ".hs") then
            pure $ Just (buildInfo exec)
        else
            pure Nothing
    lookupExposedModules = case condLibrary of
        Just condTree ->
            if elem modName (exposedModules lib) then Just (libBuildInfo lib) else Nothing
            where lib = condTreeData condTree
        Nothing -> Nothing
    lookupOtherModules = find (elem modName . otherModules) $ getBuildInfos pkg
-}

-- | Functions to retrieve and store mapping from modules to their
-- targets and extensions.

module Importify.Cabal.Target
       ( TargetMap
       , ExtensionsMap
       , getExtensionMaps
       ) where

import           Universum                             hiding (fromString)

import qualified Data.HashMap.Strict                   as Map
import           Data.List                             (partition)
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
import qualified Distribution.ModuleName               as Cabal
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (BuildInfo (..), CondTree,
                                                        Executable (..),
                                                        GenericPackageDescription (..),
                                                        Library (..), condTreeData,
                                                        exeModules, libModules)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)
import           Language.Haskell.Extension            (Extension (..),
                                                        KnownExtension (..))
import qualified Language.Haskell.Exts                 as HSE
import           Path                                  (Abs, Dir, File, Path, Rel,
                                                        fromAbsFile, parseRelDir,
                                                        parseRelFile, (</>))
import           System.Directory                      (doesFileExist)
import           System.FilePath.Posix                 (dropExtension)
import           Text.Read                             (read)

import           Importify.Syntax                      (getModuleTitle)

type TargetMap = HashMap String String
type ExtensionsMap = HashMap String [String]

getExtensionMaps :: GenericPackageDescription -> (TargetMap, ExtensionsMap)
getExtensionMaps GenericPackageDescription{..} =
    ( Map.unions $ libTargetsMaps ++ exeTargetsMaps
    , Map.unions $ libExtensionsMaps ++ exeExtensionsMaps)
  where
    (libTargetsMaps, libExtensionsMaps) = unzip $
        map (collectLibraryMaps . condTreeData) $ maybeToList condLibrary
    (exeTargetsMaps, exeExtensionsMaps) = unzip $ do
        (name, condTree) <- condExecutables
        pure $ collectExecutableMaps name $ condTreeData condTree

collectLibraryMaps :: Library -> (TargetMap, ExtensionsMap)
collectLibraryMaps lib =
    collectModuleMaps "library"
                      (map toFilePath $ libModules lib)
                      (defaultExtensions $ libBuildInfo lib)

collectExecutableMaps :: String -> Executable -> (TargetMap, ExtensionsMap)
collectExecutableMaps exeName exe =
    collectModuleMaps ("executable " ++ exeName)
                      (exePath:map toFilePath (exeModules exe))
                      (defaultExtensions $ buildInfo exe)
  where
    exePath = dropExtension $ modulePath exe

collectModuleMaps :: String -> [String] -> [Extension] -> (TargetMap, ExtensionsMap)
collectModuleMaps target mods exts =
    ( Map.fromList $ zip mods (repeat target)
    , one (target, map showExt exts)
    )

showExt :: Extension -> String
showExt (EnableExtension ext)   = show ext
showExt (DisableExtension ext)  = "No" ++ show ext
showExt (UnknownExtension name) = name

-- | This module contains common utilities for parsing .cabal files
-- and manipulating it's AST.

module Importify.Cabal
       ( getLibs
       , modulePaths
       , readCabal
       , withLibrary

       -- * Map for extensions
       , TargetMap
       , ExtensionsMap
       , getExtensionMaps
       , moduleNameToPath
       ) where

import           Universum                             hiding (fromString)

import qualified Data.HashMap.Strict                   as Map
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
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
                                                        fromRelFile, parseRelDir,
                                                        parseRelFile, (</>))
import           System.Directory                      (doesFileExist)
import           System.FilePath.Posix                 (dropExtension)
import           Text.Read                             (read)

type TargetMap = HashMap String String
type ExtensionsMap = HashMap String [String]

readCabal :: FilePath -> IO GenericPackageDescription
readCabal = readPackageDescription normal

getNodeLibInfo :: CondTree var deps Library -> BuildInfo
getNodeLibInfo = libBuildInfo . condTreeData

-- TODO: also collect for executables and make unique
-- TODO: what about version bounds?
-- | Retrieve list of all package dependencies for library of
-- given package.
getLibs :: GenericPackageDescription -> [String]
getLibs = ordNub . concat . map (map dependencyName . targetBuildDepends) . getBuildInfos

getBuildInfos :: GenericPackageDescription -> [BuildInfo]
getBuildInfos GenericPackageDescription{..} =
    maybe [] ((:[]) . libBuildInfo . condTreeData) condLibrary ++
    map (buildInfo . condTreeData . snd) condExecutables

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
                      (exePath:(map toFilePath $ exeModules exe))
                      (defaultExtensions $ buildInfo exe)
  where
    exePath = dropExtension $ modulePath exe

collectModuleMaps :: String -> [String] -> [Extension] -> (TargetMap, ExtensionsMap)
collectModuleMaps target mods exts =
    ( Map.fromList $ zip mods (repeat target)
    , one (target, map showExt exts)
    )

dependencyName :: Dependency -> String
dependencyName (Dependency PackageName{..} _) = unPackageName

cabalExtToHseExt :: Extension -> HSE.Extension
cabalExtToHseExt = {- trace ("Arg = " ++ show ext ++ "") -} read . show

isHseExt :: Extension -> Bool
isHseExt (EnableExtension NegativeLiterals) = False
isHseExt (EnableExtension Unsafe)           = False
isHseExt _                                  = True

-- | Perform given action with package library 'BuilInfo'
-- if 'Library' is present. We care only about library exposed modules
-- because only they can be imported outside that package.
withLibrary :: Applicative f
            => GenericPackageDescription
            -> (Library -> [HSE.Extension] -> f ())
            -> f ()
withLibrary GenericPackageDescription{..} action =
    whenJust condLibrary $ \treeNode ->
        let library       = condTreeData treeNode
            BuildInfo{..} = libBuildInfo library
            extensions    = filter isHseExt $ defaultExtensions ++ otherExtensions
        in action library (map cabalExtToHseExt extensions)

-- | Returns list of relative paths to each module.
modulePaths :: Path Rel Dir -> Library -> IO [Path Rel File]
modulePaths packagePath Library{..} = do
    sourcePaths <- mapM parseRelDir $ hsSourceDirs libBuildInfo
    collectModulePaths sourcePaths
  where
    exposedModulesPaths :: IO [Path Rel File]
    exposedModulesPaths = mapM (parseRelFile . (++ ".hs") . toFilePath) exposedModules

    addAbsDir :: Path Rel Dir -> [Path Rel File] -> [Path Rel File]
    addAbsDir dir = map (dir </>)

    collectModulePaths :: [Path Rel Dir] -> IO [Path Rel File]
    collectModulePaths []   = exposedModulesPaths >>=
        checkModuleExistence . addAbsDir packagePath
    collectModulePaths dirs =
        exposedModulesPaths  >>= \paths ->
        concatForM dirs        $ \dir   ->
          checkModuleExistence $ addAbsDir (packagePath </> dir) paths

    -- TODO: rewrite with either mapM or filterM
    checkModuleExistence :: [Path Rel File] -> IO [Path Rel File]
    checkModuleExistence []     = return []
    checkModuleExistence (modPath:modPaths) = do
        modExists  <- doesFileExist $ fromRelFile modPath
        if modExists
        then (modPath:) <$> checkModuleExistence modPaths
        else checkModuleExistence modPaths

showExt :: Extension -> String
showExt (EnableExtension ext)   = show ext
showExt (DisableExtension ext)  = "No" ++ show ext
showExt (UnknownExtension name) = name

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

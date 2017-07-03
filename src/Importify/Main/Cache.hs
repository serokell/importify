-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( doCache
       ) where

import           Universum

import           Data.Aeson                      (encode)
import qualified Data.ByteString.Lazy            as BS
import qualified Data.Map                        as Map
import           Data.Version                    (showVersion)

import           Distribution.Package            (PackageIdentifier (..))
import           Distribution.PackageDescription (GenericPackageDescription (packageDescription),
                                                  Library, PackageDescription (package))
import           Language.Haskell.Exts           (Extension, Module, ModuleName (..),
                                                  SrcSpanInfo)
import           Language.Haskell.Names          (writeSymbols)
import           Path                            (Abs, Dir, Path, Rel, fromAbsDir,
                                                  fromAbsFile, parseAbsDir, parseRelDir,
                                                  parseRelFile, (</>))
import           System.Directory                (createDirectoryIfMissing,
                                                  getCurrentDirectory, listDirectory,
                                                  removeDirectoryRecursive)
import           System.FilePath                 (dropExtension, dropFileName,
                                                  takeFileName)
import           Turtle                          (cd, shell)

import           Importify.Cabal                 (getExtensionMaps, getLibs, getLibs,
                                                  modulePaths, readCabal,
                                                  splitOnExposedAndOther, withLibrary)
import           Importify.CPP                   (parseModuleFile)
import           Importify.Paths                 (cacheDir, cachePath, extensionsFile,
                                                  guessCabalName, modulesFile,
                                                  symbolsPath, targetsFile)
import           Importify.Resolution            (resolveModules)

-- | Caches packages information into local .importify directory.
doCache :: FilePath -> Bool -> [String] -> IO ()
doCache filepath preserve overrideDependencies = do
    projectCabalDesc <- readCabal filepath

    curDir           <- getCurrentDirectory
    projectPath      <- parseAbsDir curDir
    let importifyPath = projectPath </> cachePath
    let importifyDir  = fromAbsDir importifyPath

    -- TODO: error if not inside project directory?
    createDirectoryIfMissing True importifyDir  -- creates ./.importify
    cd $ fromString cacheDir    -- cd to ./.importify/

    -- Extension maps
    let (targetMaps, extensionMaps) = getExtensionMaps projectCabalDesc
    BS.writeFile targetsFile    $ encode targetMaps
    BS.writeFile extensionsFile $ encode extensionMaps

    -- Libraries
    let projectName = dropExtension $ takeFileName filepath
    let fetchedLibs = filter (\p -> p /= "base" && p /= projectName) $ getLibs projectCabalDesc
    let libs        = if null overrideDependencies
                      then fetchedLibs
                      else overrideDependencies
    print libs

    -- download & unpack sources, then cache and delete
    dependenciesResolutionMaps <- forM libs $
        collectDependenciesResolution importifyPath preserve

    let PackageIdentifier{..} = package $ packageDescription projectCabalDesc
    projectResolutionMap <- createProjectCache projectCabalDesc
                                               projectPath
                                               (importifyPath </> symbolsPath)
                                               projectName
                                               (projectName ++ '-':showVersion pkgVersion)

    let importsMap = Map.unions (projectResolutionMap:dependenciesResolutionMaps)
    BS.writeFile modulesFile $ encode importsMap

    cd ".."

collectDependenciesResolution :: Path Abs Dir
                              -> Bool
                              -> String
                              -> IO (Map String String)
collectDependenciesResolution importifyPath preserve libName = do
        _exitCode            <- shell ("stack unpack " <> toText libName) empty
        localPackages        <- listDirectory (fromAbsDir importifyPath)
        let maybePackage      = find (libName `isPrefixOf`) localPackages
        let downloadedPackage = fromMaybe (error "Package wasn't downloaded!")
                                          maybePackage  -- TODO: this is not fine

        packagePath              <- parseRelDir downloadedPackage
        let downloadedPackagePath = importifyPath </> packagePath
        let cabalFileName         = guessCabalName libName
        packageCabalDesc         <- readCabal $ fromAbsFile
                                              $ downloadedPackagePath </> cabalFileName

        let symbolsCachePath = importifyPath </> symbolsPath
        packageModules <- createProjectCache packageCabalDesc
                                             downloadedPackagePath
                                             symbolsCachePath
                                             libName
                                             downloadedPackage

        unless preserve $  -- TODO: use bracket here
            removeDirectoryRecursive downloadedPackage

        pure packageModules

createProjectCache :: GenericPackageDescription
                   -> Path Abs Dir
                   -> Path Abs Dir
                   -> String
                   -> String
                   -> IO (Map String String)
createProjectCache packageCabalDesc packagePath symbolsCachePath libName packageName =
    withLibrary packageCabalDesc $ \library cabalExtensions -> do
        -- creates ./.importify/symbols/<package>/
        packageNamePath     <- parseRelDir packageName
        let packageCachePath = symbolsCachePath </> packageNamePath
        createDirectoryIfMissing True $ fromAbsDir packageCachePath

        (errors, libModules) <- parsedModulesWithErrors packagePath
                                                        library
                                                        cabalExtensions
        reportErrorsIfAny errors libName

        let (exposedModules, otherModules) = splitOnExposedAndOther library libModules
        let resolvedModules = resolveModules exposedModules otherModules

        packageModules <- forM resolvedModules $ \(ModuleName () moduleTitle, resolvedSymbols) -> do
            modSymbolsPath     <- parseRelFile $ moduleTitle ++ ".symbols"
            let moduleCachePath = packageCachePath </> modSymbolsPath

            -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
            writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

            pure (moduleTitle, packageName)

        pure $ Map.fromList packageModules

parsedModulesWithErrors :: Path Abs Dir
                        -> Library
                        -> [Extension]
                        -> IO ([Text], [Module SrcSpanInfo])
parsedModulesWithErrors packagePath library cabalExtensions = do
    modPaths   <- modulePaths packagePath library
    modEithers <- mapM (parseModuleFile cabalExtensions) modPaths
    pure $ partitionEithers modEithers

reportErrorsIfAny :: [Text] -> String -> IO ()
reportErrorsIfAny errors libName = whenNotNull errors $ \messages -> do
    putText $ " * Next errors occured during caching of package: " <> toText libName
    forM_ messages putText

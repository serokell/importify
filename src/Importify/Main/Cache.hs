-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( doCache
       ) where

import           Universum

import           Data.Aeson                      (encode)
import qualified Data.ByteString.Lazy            as BS
import qualified Data.Map                        as Map
import qualified Data.Text                       as T
import           Data.Version                    (showVersion)

import           Distribution.Package            (PackageIdentifier (..))
import           Distribution.PackageDescription (GenericPackageDescription (packageDescription),
                                                  Library, PackageDescription (package))
import           Fmt                             (Builder, blockListF, build, fmt, fmtLn,
                                                  indent, listF)
import           Language.Haskell.Exts           (Extension, Module, ModuleName (..),
                                                  SrcSpanInfo)
import           Language.Haskell.Names          (writeSymbols)
import           Path                            (Abs, Dir, Path, fromAbsDir, fromAbsFile,
                                                  parseAbsDir, parseRelDir, parseRelFile,
                                                  (</>))
import           System.Directory                (createDirectoryIfMissing,
                                                  getCurrentDirectory, listDirectory,
                                                  removeDirectoryRecursive)
import           System.FilePath                 (dropExtension, takeExtension,
                                                  takeFileName)
import           Turtle                          (cd, shell)

import           Extended.System.Wlog            (printInfo, printWarning)
import           Importify.Cabal                 (getExtensionMaps, libraryExtensions,
                                                  libraryIncludeDirs, modulePaths,
                                                  packageDependencies, readCabal,
                                                  splitOnExposedAndOther, withLibrary)
import           Importify.CPP                   (parseModuleFile)
import           Importify.ParseException        (ModuleParseException)
import           Importify.ParseException        (reportErrorsIfAny)
import           Importify.Paths                 (cacheDir, cachePath, doInsideDir,
                                                  extensionsFile, guessCabalName,
                                                  modulesFile, symbolsPath, targetsFile)
import           Importify.Resolution            (resolveModules)

-- | Caches packages information into local .importify directory.
doCache :: Bool -> [String] -> IO ()
doCache preserveSources [] = do
    thisDirectory <- getCurrentDirectory
    thisDirNodes  <- listDirectory thisDirectory
    let cabalFiles = filter ((== ".cabal") . takeExtension) thisDirNodes
    case cabalFiles of
        [] -> printWarning "No .cabal file in this directory! Aborting. Please, \
                           \run this command from your project root directory."
        (cabalFile:_) -> cacheProject preserveSources cabalFile
doCache preserveSources explicitDependencies = do
    printInfo "Using explicitly specifined list of dependencies for caching..."
    thisDirectory    <- getCurrentDirectory
    projectPath      <- parseAbsDir thisDirectory
    let importifyPath = projectPath </> cachePath
    doInsideDir importifyPath $
        () <$ unpackAndCacheDependencies importifyPath
                                         preserveSources
                                         explicitDependencies

cacheProject :: Bool -> FilePath -> IO ()
cacheProject preserveSources cabalFile = do
    -- TODO: remove code duplication
    thisDirectory    <- getCurrentDirectory
    projectPath      <- parseAbsDir thisDirectory
    cabalFilePath    <- parseRelFile cabalFile
    let cabalPath     = fromAbsFile $ projectPath </> cabalFilePath
    let importifyPath = projectPath </> cachePath

    doInsideDir importifyPath $ do
        projectCabalDesc <- readCabal cabalPath

        -- Extension maps
        let (targetMaps, extensionMaps) = getExtensionMaps projectCabalDesc
        BS.writeFile targetsFile    $ encode targetMaps
        BS.writeFile extensionsFile $ encode extensionMaps

        -- Libraries
        let projectName = dropExtension $ takeFileName cabalFile
        let libraries   = sort
                        $ filter (\p -> p /= "base" && p /= projectName)
                        $ packageDependencies projectCabalDesc
        printInfo $ fmt $ "Downloading dependencies: " <> listF libraries

        -- download & unpack sources, then cache and delete
        dependenciesResolutionMaps <- unpackAndCacheDependencies importifyPath
                                                                 preserveSources
                                                                 libraries

        let PackageIdentifier{..} = package $ packageDescription projectCabalDesc
        projectResolutionMap <- createProjectCache projectCabalDesc
                                                   projectPath
                                                   (importifyPath </> symbolsPath)
                                                   projectName
                                                   (projectName ++ '-':showVersion pkgVersion)

        let importsMap = Map.unions (projectResolutionMap:dependenciesResolutionMaps)
        BS.writeFile modulesFile $ encode importsMap

unpackAndCacheDependencies :: Path Abs Dir
                             -> Bool
                             -> [String]
                             -> IO [Map String String]
unpackAndCacheDependencies importifyPath preserveSources dependencies =
    forM dependencies $
        collectDependenciesResolution importifyPath preserveSources

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
    withLibrary packageCabalDesc $ \library -> do
        -- creates ./.importify/symbols/<package>/
        packageNamePath     <- parseRelDir packageName
        let packageCachePath = symbolsCachePath </> packageNamePath
        createDirectoryIfMissing True $ fromAbsDir packageCachePath

        (errors, libModules) <- parsedModulesWithErrors packagePath
                                                        library
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

parsedModulesWithErrors :: Path Abs Dir  -- ^ Path like @~/.../.importify/containers-0.5@
                        -> Library
                        -> IO ([ModuleParseException], [Module SrcSpanInfo])
parsedModulesWithErrors packagePath library = do
    includeDirPaths <- mapM parseRelDir $ libraryIncludeDirs library
    let includeDirs  = map (fromAbsDir . (packagePath </>)) includeDirPaths
    let extensions   = libraryExtensions  library
    pathsToModules  <- modulePaths packagePath library

    modEithers <- mapM (parseModuleFile extensions includeDirs) pathsToModules
    pure $ partitionEithers modEithers

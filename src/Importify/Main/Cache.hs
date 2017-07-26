-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( doCache
       ) where

import           Universum

import           Data.Aeson                      (decodeStrict, encode)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Map                        as M
import           Data.Version                    (showVersion)

import           Distribution.Package            (PackageIdentifier (..))
import           Distribution.PackageDescription (GenericPackageDescription (packageDescription),
                                                  Library (..),
                                                  PackageDescription (package))
import           Fmt                             (Builder, blockListF, build, fmt, fmtLn,
                                                  indent, listF)
import           Language.Haskell.Exts           (Module, ModuleName (..), SrcSpanInfo)
import           Language.Haskell.Names          (writeSymbols)
import           Path                            (Abs, Dir, File, Path, Rel, fromAbsDir,
                                                  fromAbsFile, fromRelDir, fromRelFile,
                                                  parseRelDir, parseRelFile, (</>))
import           System.Directory                (createDirectoryIfMissing,
                                                  doesDirectoryExist, doesFileExist,
                                                  getCurrentDirectory,
                                                  removeDirectoryRecursive)
import           System.FilePath                 (dropExtension)
import           Turtle                          (shell)

import           Extended.Data.List              (mapMaybeM)
import           Extended.System.Wlog            (printDebug, printInfo, printWarning)
import           Importify.Cabal                 (getExtensionMaps, libraryExtensions,
                                                  libraryIncludeDirs, modulePaths,
                                                  packageDependencies, readCabal,
                                                  splitOnExposedAndOther,
                                                  withHarmlessExtensions, withLibrary)
import           Importify.ParseException        (ModuleParseException, reportErrorsIfAny)
import           Importify.Paths                 (cachePath, doInsideDir, extensionsFile,
                                                  findCabalFile, getCurrentPath,
                                                  modulesFile, symbolsPath, targetsFile)
import           Importify.Preprocessor          (parseModuleWithPreprocessor)
import           Importify.Resolution            (resolveModules)
import           Importify.Stack                 (ghcIncludePath, stackListDependencies,
                                                  upgradeWithVersions)

-- | Caches packages information into local .importify directory.
doCache :: Bool -> [String] -> IO ()
doCache preserveSources [] = do
    thisDirectory <- getCurrentDirectory
    findCabalFile thisDirectory >>= \case
        Nothing -> printWarning "No .cabal file in this directory! Aborting. Please, \
                                \run this command from your project root directory."
        Just cabalFile -> cacheProject preserveSources cabalFile
doCache preserveSources explicitDependencies = do
    printInfo "Using explicitly specifined list of dependencies for caching..."
    projectPath      <- getCurrentPath
    let importifyPath = projectPath </> cachePath
    doInsideDir importifyPath $
        () <$ unpackAndCacheDependencies importifyPath
                                         preserveSources
                                         explicitDependencies

cacheProject :: Bool -> Path Rel File -> IO ()
cacheProject preserveSources cabalFile = do
    -- TODO: remove code duplication
    projectPath      <- getCurrentPath
    let cabalPath     = fromAbsFile $ projectPath </> cabalFile
    let importifyPath = projectPath </> cachePath

    doInsideDir importifyPath $ do
        projectCabalDesc <- readCabal cabalPath

        -- Maps from full path to module
        (targetMaps, extensionMaps) <- getExtensionMaps projectPath
                                                        projectCabalDesc
        LBS.writeFile targetsFile    $ encode targetMaps
        LBS.writeFile extensionsFile $ encode extensionMaps

        -- Libraries
        libVersions    <- stackListDependencies
        let projectName = dropExtension $ fromRelFile cabalFile
        let libraries   = sort
                        $ upgradeWithVersions libVersions
                        $ filter (/= projectName)
                        $ packageDependencies projectCabalDesc
        printInfo $ fmt $ "Downloading dependencies: " <> listF libraries

        -- download & unpack sources, then cache and delete
        dependenciesResolutionMaps <- unpackAndCacheDependencies importifyPath
                                                                 preserveSources
                                                                 libraries

        let PackageIdentifier{..} = package $ packageDescription projectCabalDesc
        let fullProjectName       = toText $ projectName ++ '-':showVersion pkgVersion
        projectResolutionMap <- createProjectCache projectCabalDesc
                                                   projectPath
                                                   (importifyPath </> symbolsPath)
                                                   fullProjectName
                                                   True

        let importsMap = M.unions (projectResolutionMap:dependenciesResolutionMaps)
        updateModulesMap importsMap

unpackAndCacheDependencies :: Path Abs Dir
                           -> Bool
                           -> [String]
                           -> IO [Map String Text]
unpackAndCacheDependencies importifyPath preserveSources dependencies =
    mapM libraryResolver =<< mapMaybeM isNotUnpacked dependencies
  where
    isNotUnpacked :: String -> IO (Maybe Text)
    isNotUnpacked libName = do
        libraryPath       <- parseRelDir libName
        let libSymbolsPath = symbolsPath </> libraryPath
        isAlreadyUnpacked <- doesDirectoryExist (fromRelDir libSymbolsPath)

        let textLibName    = toText libName
        if isAlreadyUnpacked then do
            printDebug $ textLibName <> " is already cached"
            return Nothing
        else
            return $ Just textLibName

    libraryResolver :: Text -> IO (Map String Text)
    libraryResolver = collectDependenciesResolution importifyPath preserveSources

collectDependenciesResolution :: Path Abs Dir
                              -> Bool
                              -> Text
                              -> IO (Map String Text)
collectDependenciesResolution importifyPath preserve libName = do
    _exitCode <- shell ("stack unpack " <> libName) empty

    let stringLibName         = toString libName
    packagePath              <- parseRelDir stringLibName
    let downloadedPackagePath = importifyPath </> packagePath
    mCabalFileName           <- findCabalFile stringLibName

    let cabalFileName = fromMaybe (error $ "No .cabal file inside: " <> libName)
                                  mCabalFileName
    packageCabalDesc <- readCabal
                      $ fromAbsFile
                      $ downloadedPackagePath </> cabalFileName

    let symbolsCachePath = importifyPath </> symbolsPath
    packageModules <- createProjectCache packageCabalDesc
                                         downloadedPackagePath
                                         symbolsCachePath
                                         libName
                                         False

    unless preserve $  -- TODO: use bracket here
        removeDirectoryRecursive stringLibName

    pure packageModules

createProjectCache :: GenericPackageDescription
                   -> Path Abs Dir
                   -> Path Abs Dir
                   -> Text
                   -> Bool
                   -> IO (Map String Text)
createProjectCache
    packageCabalDesc
    packagePath
    symbolsCachePath
    packageName
    keepOtherModules
  = withLibrary packageCabalDesc $ \library -> do
        -- creates ./.importify/symbols/<package>/
        packageNamePath     <- parseRelDir (toString packageName)
        let packageCachePath = symbolsCachePath </> packageNamePath
        createDirectoryIfMissing True $ fromAbsDir packageCachePath

        (errors, libModules) <- parsedModulesWithErrors packagePath
                                                        library
        reportErrorsIfAny errors packageName

        let (exposedModules, otherModules) = splitOnExposedAndOther library libModules
        let resolvedModules = if keepOtherModules
                              then resolveModules (exposedModules ++ otherModules) []
                              else resolveModules exposedModules otherModules

        packageModules <- forM resolvedModules $ \(ModuleName () moduleTitle, resolvedSymbols) -> do
            modSymbolsPath     <- parseRelFile $ moduleTitle ++ ".symbols"
            let moduleCachePath = packageCachePath </> modSymbolsPath

            -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
            writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

            pure (moduleTitle, packageName)

        pure $ M.fromList packageModules

packageLibraryModules :: Path Abs Dir
                      -> Library
                      -> IO [Path Abs File]
packageLibraryModules packagePath Library{..} =
    modulePaths packagePath libBuildInfo (Left exposedModules)

parsedModulesWithErrors :: Path Abs Dir  -- ^ Path like @~/.../.importify/containers-0.5@
                        -> Library
                        -> IO ([ModuleParseException], [Module SrcSpanInfo])
parsedModulesWithErrors packagePath library = do
    -- paths to all modules
    pathsToModules <- packageLibraryModules packagePath library

    -- get include directories for cpphs
    includeDirPaths   <- mapM parseRelDir $ libraryIncludeDirs library
    let pkgIncludeDirs = map (fromAbsDir . (packagePath </>)) includeDirPaths
    ghcIncludeDir     <- toList <$> runMaybeT ghcIncludePath
    let includeDirs    = pkgIncludeDirs ++ ghcIncludeDir

    -- get extensions
    let extensions = withHarmlessExtensions $ libraryExtensions library

    let moduleParser = parseModuleWithPreprocessor extensions includeDirs
    partitionEithers <$> mapM moduleParser pathsToModules

updateModulesMap :: Map String Text -> IO ()
updateModulesMap newCachedModules = do
    isModulesFileExist <- doesFileExist modulesFile
    existingImportsMap <- if isModulesFileExist
                          then fromMaybe (error "Invalid modules file") . decodeStrict
                           <$> BS.readFile modulesFile
                          else pure mempty
    let mergedMaps = newCachedModules `M.union` existingImportsMap
    LBS.writeFile modulesFile $ encode mergedMaps

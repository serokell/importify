{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( doCache
       ) where

import           Universum

import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString.Lazy            as LBS (writeFile)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (notElem)
import           Data.Maybe                      (fromJust)
import           Data.Version                    (showVersion)

import           Distribution.PackageDescription (BuildInfo (includeDirs), GenericPackageDescription (packageDescription),
                                                  PackageDescription (package),
                                                  condLibrary, condTreeData, cppOptions,
                                                  libBuildInfo)
import           Fmt                             (Builder, blockListF, build, fmt, fmtLn,
                                                  indent, listF, ( #| ), (|#))
import           Language.Haskell.Exts           (Module, ModuleName (..), SrcSpanInfo)
import           Language.Haskell.Names          (writeSymbols)
import           Path                            (Abs, Dir, File, Path, Rel, fromAbsDir,
                                                  fromAbsFile, fromRelDir, fromRelFile,
                                                  parseAbsFile, parseRelDir, parseRelFile,
                                                  (</>))
import           System.Directory                (createDirectoryIfMissing,
                                                  doesDirectoryExist, getCurrentDirectory,
                                                  removeDirectoryRecursive)
import           System.FilePath                 (dropExtension)
import           Turtle                          (shell)

import           Extended.System.Wlog            (printDebug, printInfo, printWarning)
import           Importify.Cabal                 (ModulesMap, TargetId (LibraryId),
                                                  buildInfoExtensions,
                                                  extractTargetBuildInfo, getMapBundle,
                                                  packageDependencies, packageTargets,
                                                  readCabal, targetIdDir,
                                                  withHarmlessExtensions)
import           Importify.ParseException        (ModuleParseException, reportErrorsIfAny)
import           Importify.Paths                 (cachePath, decodeFileOrMempty,
                                                  doInsideDir, extensionsFile,
                                                  findCabalFile, getCurrentPath,
                                                  modulesFile, symbolsPath, targetsFile)
import           Importify.Preprocessor          (parseModuleWithPreprocessor)
import           Importify.Resolution            (resolveModules)
import           Importify.Stack                 (LocalPackages (..), QueryPackage (..),
                                                  RemotePackages (..), ghcIncludePath,
                                                  pkgName, stackListDependencies,
                                                  stackListPackages, upgradeWithVersions)
import           Importify.Syntax                (debugAST, debugLabel, getModuleTitle)

-- | Caches packages information into local .importify directory.
doCache :: Bool -> [Text] -> IO ()
doCache preserveSources [] = do
    (localPackages@(LocalPackages locals), remotePackages) <- stackListPackages
    if null locals
    then printWarning "No packages found :( This could happen due to next reasons:\n\
                      \    1. Not running from project root directory.\n\
                      \    2. 'stack query' command failure.\n\
                      \    3. Our failure in parsing 'stack query' output."
    else cacheProject preserveSources localPackages remotePackages
doCache preserveSources explicitDependencies = do
    printInfo "Using explicitly specified list of dependencies for caching..."
    projectPath      <- getCurrentPath
    let importifyPath = projectPath </> cachePath
    doInsideDir importifyPath $
        () <$ cacheDependenciesWith importifyPath
                                    identity
                                    (unpackCacher preserveSources)
                                    explicitDependencies

cacheProject :: Bool -> LocalPackages -> RemotePackages -> IO ()
cacheProject preserveSources (LocalPackages locals) (RemotePackages remotes) = do
    -- [IMRF-70]: Move this to ReaderT config
    projectPath <- getCurrentPath
    let importifyPath = projectPath </> cachePath

    localDescriptions   <- mapM localPackageDescription locals
    hackageDependencies <- extractHackageDependencies localDescriptions
                                                      (locals ++ remotes)

    doInsideDir importifyPath $ do
        -- 1. Unpack hackage dependencies then cache them
        printInfo $ "Caching total "#|length hackageDependencies|#
                    " dependencies from Hackage: "#|listF hackageDependencies|#""
        hackageMaps <- cacheDependenciesWith importifyPath
                                             identity
                                             (unpackCacher preserveSources)
                                             hackageDependencies

        -- 2. Unpack all remote non-hackage dependencies (e.g. from GitHub)
        remoteMaps <- cacheDependenciesWith importifyPath
                                            pkgName
                                            remoteCacher
                                            remotes

        -- 3. Unpack finally all local packages
        localMaps <- forM locals $ \localPackage -> do
            printInfo $ "Caching package: " <> pkgName localPackage
            cachePackage importifyPath
                         (qpPath localPackage)
                         (pkgName localPackage)
                         True

        updateModulesMap $ (HM.unions localMaps)
                `HM.union` (HM.unions remoteMaps)
                `HM.union` (HM.unions hackageMaps)

localPackageDescription :: QueryPackage -> IO GenericPackageDescription
localPackageDescription QueryPackage{..} = do
    Just cabalPath <- findCabalFile qpPath
    let cabalFile   = fromAbsFile cabalPath
    readCabal cabalFile

extractHackageDependencies :: [GenericPackageDescription]
                           -> [QueryPackage]
                           -> IO [Text]
extractHackageDependencies descriptions (map pkgName -> nonHackagePackages) = do
    libVersions     <- stackListDependencies
    let versifier    = upgradeWithVersions libVersions
                     . map toText
                     . packageDependencies
    let dependencies = concatMap versifier descriptions

    let uniqueDependencies = sort
                           $ filter (`notElem` nonHackagePackages)
                           $ hashNub dependencies

    return uniqueDependencies

-- | Collect cache of list of given dependencies. If given dependency
-- is already cached then it's ignored and debug message is printed.
cacheDependenciesWith :: forall d .
                         Path Abs Dir
                      -- ^ Absolute path to .importify folder
                      -> (d -> Text)
                      -- ^ How to get dependency name?
                      -> (Path Abs Dir -> d -> IO ModulesMap)
                      -- ^ How to cache dependency (unpack for StackDependency)
                      -> [d]
                      -- ^ List of dependencies that should be cached
                      -> IO [ModulesMap]
cacheDependenciesWith
    importifyPath
    dependencyName
    resolver
  = go
  where
    go :: [d] -> IO [ModulesMap]
    go []     = return []
    go (d:ds) = do
        let depName = dependencyName d
        isAlreadyCached depName >>= \case
            True  -> printDebug (depName <> " is already cached") *> go ds
            False -> liftM2 (:) (dependencyResolver d) (go ds)

    isAlreadyCached :: Text -> IO Bool
    isAlreadyCached libName = do
        libraryPath       <- parseRelDir $ toString libName
        let libSymbolsPath = symbolsPath </> libraryPath
        doesDirectoryExist (fromRelDir libSymbolsPath)

    dependencyResolver :: d -> IO ModulesMap
    dependencyResolver = resolver importifyPath

-- | This function is passed to 'cacheDependenciesWith' for Hackage dependencies.
unpackCacher
    :: Bool
    -> Path Abs Dir
    -> Text
    -> IO ModulesMap
unpackCacher preserve importifyPath libName = do
    _exitCode <- shell ("stack unpack " <> libName) empty

    let stringLibName         = toString libName
    packagePath              <- parseRelDir stringLibName
    let downloadedPackagePath = importifyPath </> packagePath

    packageModules <- cachePackage importifyPath
                                   downloadedPackagePath
                                   libName
                                   False

    unless preserve $  -- TODO: use bracket here
        removeDirectoryRecursive stringLibName

    pure packageModules

-- | This function is passed to 'cacheDependenciesWith' for 'RemotePackages'.
remoteCacher :: Path Abs Dir -> QueryPackage -> IO ModulesMap
remoteCacher importifyPath package = do
    let packageName = pkgName package
    printInfo $ "Caching remote package: " <> packageName
    cachePackage importifyPath (qpPath package) packageName False

-- | Find .cabal file by given path to package and then collect 'ModulesMap'.
cachePackage :: Path Abs Dir  -- ^ Path to .importify folder
             -> Path Abs Dir  -- ^ Path to package
             -> Text          -- ^ Package name
             -> Bool          -- ^ Is package itself is caching
             -> IO ModulesMap
cachePackage importifyPath packagePath libName isWorkingProject = do
    -- finding path to unpacked package .cabal file
    mCabalFileName   <- findCabalFile packagePath
    let cabalFileName = fromMaybe (error $ "No .cabal file inside: " <> libName)
                                  mCabalFileName

    packageCabalDesc <- readCabal $ fromAbsFile cabalFileName

    let symbolsCachePath = importifyPath </> symbolsPath
    createPackageCache packageCabalDesc
                       packagePath
                       symbolsCachePath
                       libName
                       isWorkingProject

-- | Creates @./.impority/symbols/<package> folder where all symbols
-- for given library stored. This function used for both library packages
-- and project package itself.
createPackageCache :: GenericPackageDescription
                   -- ^ Package descriptions
                   -> Path Abs Dir
                   -- ^ Path to package root
                   -> Path Abs Dir
                   -- ^ Absolute path to .importify/symbols
                   -- [IMRF-70]: this should be removed after introducing ReaderT
                   -> Text
                   -- ^ Package name with version
                   -> Bool
                   -- ^ True if project itself is caching now
                   -> IO ModulesMap
createPackageCache
    packageCabalDesc
    packagePath
    symbolsCachePath
    packageName
    isWorkingProject
  = do
    -- creates ./.importify/symbols/<package>/
    packageNamePath     <- parseRelDir (toString packageName)
    let packageCachePath = symbolsCachePath </> packageNamePath
    createDirectoryIfMissing True $ fromAbsDir packageCachePath

    -- Maps from full path to target and from target to list of extensions
    (targetMap, extensionsMap) <- getMapBundle packagePath packageCabalDesc


    -- Cache MapBundle only for working project
    when isWorkingProject $ do
        LBS.writeFile targetsFile    $ encodePretty targetMap
        LBS.writeFile extensionsFile $ encodePretty extensionsMap

    -- [IMRF-70]: move this to ReaderT config
    ghcIncludeDir <- runMaybeT ghcIncludePath

    let moduleToTargetPairs = HM.toList targetMap
    let targetIds = if isWorkingProject
                    then packageTargets packageCabalDesc
                    else [LibraryId]
    concatForM targetIds $ \targetId -> do
        let thisTargetModules = map fst
                              $ filter ((== targetId) . snd) moduleToTargetPairs
        targetPaths <- mapM parseAbsFile thisTargetModules

        let targetInfo = fromMaybe (error $ "No such target: " <> show targetId)
                       $ extractTargetBuildInfo targetId packageCabalDesc

        (errors, targetModules) <- parseTargetModules packagePath
                                                      targetPaths
                                                      targetInfo
                                                      ghcIncludeDir
        let targetDirectory = targetIdDir targetId
        reportErrorsIfAny errors (packageName <> ":" <> targetDirectory)

        targetPath           <- parseRelDir $ toString targetDirectory
        let packageTargetPath = packageCachePath </> targetPath
        createDirectoryIfMissing True $ fromAbsDir packageTargetPath

        let moduleToPathMap = HM.fromList $ map (first getModuleTitle) targetModules
        let resolvedModules = resolveModules $ map fst targetModules
        fmap HM.fromList $ forM resolvedModules $ \( ModuleName () moduleTitle
                                                  , resolvedSymbols) -> do
            modSymbolsPath     <- parseRelFile $ moduleTitle ++ ".symbols"
            let moduleCachePath = packageTargetPath </> modSymbolsPath

            -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
            writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

            let modulePath = fromMaybe (error $ toText $ "Unknown module: " ++ moduleTitle)
                           $ HM.lookup moduleTitle moduleToPathMap
            pure (fromAbsFile modulePath, (packageName, moduleTitle))

parseTargetModules :: Path Abs Dir    -- ^ Path like @~/.../.importify/containers-0.5@
                   -> [Path Abs File] -- ^ Paths to modules
                   -> BuildInfo       -- ^ BuildInfo of current target
                   -> Maybe FilePath  -- ^ Path to ghc bundled headers
                   -> IO ( [ModuleParseException]
                         , [(Module SrcSpanInfo, Path Abs File)]
                         )
parseTargetModules packagePath pathsToModules targetInfo ghcIncludeDir = do
    -- get include directories for cpphs
    includeDirPaths   <- mapM parseRelDir $ includeDirs targetInfo
    let pkgIncludeDirs = map (fromAbsDir . (packagePath </>)) includeDirPaths

    let includeDirs = pkgIncludeDirs ++ toList ghcIncludeDir
    let extensions  = withHarmlessExtensions $ buildInfoExtensions targetInfo

    let moduleParser path = do
            parseRes <- parseModuleWithPreprocessor extensions
                                                    includeDirs
                                                    path
            return $ second (, path) parseRes

    partitionEithers <$> mapM moduleParser pathsToModules

updateModulesMap :: ModulesMap -> IO ()
updateModulesMap newCachedModules = do
    existingImportsMap <- decodeFileOrMempty modulesFile return
    let mergedMaps      = newCachedModules `HM.union` existingImportsMap
    LBS.writeFile modulesFile $ encodePretty mergedMaps

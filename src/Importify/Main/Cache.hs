{-# LANGUAGE TupleSections #-}

-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( doCache
       ) where

import           Universum

import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString.Lazy            as LBS (writeFile)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (notElem)
import           Data.Version                    (showVersion)

import           Distribution.PackageDescription (BuildInfo (includeDirs), GenericPackageDescription (packageDescription),
                                                  PackageDescription (package))
import           Fmt                             (Builder, blockListF, build, fmt, fmtLn,
                                                  indent, listF)
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

import           Extended.Data.List              (mapMaybeM)
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
import           Importify.Stack                 (LocalPackage (..), ghcIncludePath,
                                                  pkgName, stackListDependencies,
                                                  stackListPackages, upgradeWithVersions)
import           Importify.Syntax                (debugAST, getModuleTitle)

-- | Caches packages information into local .importify directory.
doCache :: Bool -> [String] -> IO ()
doCache preserveSources [] = stackListPackages >>= \case
    [] -> printWarning "No packages found :( This could happen due to next reasons:\n\
                       \    1. Not running from project root directory.\n\
                       \    2. 'stack query' command failure.\n\
                       \    3. Our failure in parsing 'stack query' output."
    packages -> do
        -- [IMRF-70]: Move this to ReaderT config
        projectPath <- getCurrentPath
        let importifyPath = projectPath </> cachePath
        forM_ packages $ \package -> do
            printInfo $ "Caching package: " <> pkgName package
            cacheProject preserveSources importifyPath (map (toString.pkgName) packages) package

doCache preserveSources explicitDependencies = do
    printInfo "Using explicitly specifined list of dependencies for caching..."
    projectPath      <- getCurrentPath
    let importifyPath = projectPath </> cachePath
    doInsideDir importifyPath $
        () <$ unpackAndCacheDependencies importifyPath
                                         preserveSources
                                         explicitDependencies

cacheProject :: Bool -> Path Abs Dir -> [String] -> LocalPackage -> IO ()
cacheProject preserveSources importifyPath localPackages pkg@LocalPackage{..} = do
    Just cabalPath <- findCabalFile lpPath
    let cabalFile   = fromAbsFile cabalPath

    doInsideDir importifyPath $ do
        projectCabalDesc <- readCabal cabalFile

        -- Libraries
        -- [IMRF-70]: Move to ReaderT config
        libVersions    <- stackListDependencies
        let packageName = pkgName pkg
        let libraries   = sort
                        $ filter (`notElem` localPackages)
                        $ upgradeWithVersions libVersions
                        $ packageDependencies projectCabalDesc
        printInfo $ fmt $ "Caching next dependencies: " <> listF libraries

        -- download & unpack sources, then cache and delete
        dependenciesResolutionMaps <- unpackAndCacheDependencies importifyPath
                                                                 preserveSources
                                                                 libraries

        projectResolutionMap <- createProjectCache projectCabalDesc
                                                   lpPath
                                                   (importifyPath </> symbolsPath)
                                                   (packageTargets projectCabalDesc)
                                                   packageName
                                                   True

        let importsMap = HM.unions (projectResolutionMap:dependenciesResolutionMaps)
        updateModulesMap importsMap

unpackAndCacheDependencies :: Path Abs Dir
                           -> Bool
                           -> [String]
                           -> IO [ModulesMap]
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

    libraryResolver :: Text -> IO ModulesMap
    libraryResolver = collectDependenciesResolution importifyPath preserveSources

collectDependenciesResolution :: Path Abs Dir
                              -> Bool
                              -> Text
                              -> IO ModulesMap
collectDependenciesResolution importifyPath preserve libName = do
    _exitCode <- shell ("stack unpack " <> libName) empty

    let stringLibName         = toString libName
    packagePath              <- parseRelDir stringLibName
    let downloadedPackagePath = importifyPath </> packagePath

    -- finding path to unpacked package .cabal file
    mCabalFileName   <- findCabalFile downloadedPackagePath
    let cabalFileName = fromMaybe (error $ "No .cabal file inside: " <> libName)
                                  mCabalFileName
    packageCabalDesc <- readCabal $ fromAbsFile cabalFileName

    let symbolsCachePath = importifyPath </> symbolsPath
    packageModules <- createProjectCache packageCabalDesc
                                         downloadedPackagePath
                                         symbolsCachePath
                                         [LibraryId]
                                         libName
                                         False

    unless preserve $  -- TODO: use bracket here
        removeDirectoryRecursive stringLibName

    pure packageModules

-- | Creates @./.impority/symbols/<package> folder where all symbols
-- for given library stored. This function used for both library packages
-- and project package itself.
createProjectCache :: GenericPackageDescription
                   -- ^ Package descriptions
                   -> Path Abs Dir
                   -- ^ Path to package root
                   -> Path Abs Dir
                   -- ^ Absolute path to .importify/symbols
                   -- [IMRF-70]: this should be removed after introducing ReaderT
                   -> [TargetId]
                   -- ^ List of targets that should be cached
                   -> Text
                   -- ^ Package name with version
                   -> Bool
                   -- ^ True if project itself is cached
                   -> IO ModulesMap
createProjectCache
    packageCabalDesc
    packagePath
    symbolsCachePath
    targetIds
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

    let includeDirs  = pkgIncludeDirs ++ toList ghcIncludeDir
    let extensions   = withHarmlessExtensions $ buildInfoExtensions targetInfo

    let moduleParser path = do
            parseRes <- parseModuleWithPreprocessor extensions
                                                    includeDirs
                                                    path
            return $ second (,path) parseRes

    partitionEithers <$> mapM moduleParser pathsToModules

updateModulesMap :: ModulesMap -> IO ()
updateModulesMap newCachedModules = do
    existingImportsMap <- decodeFileOrMempty modulesFile return
    let mergedMaps      = newCachedModules `HM.union` existingImportsMap
    LBS.writeFile modulesFile $ encodePretty mergedMaps

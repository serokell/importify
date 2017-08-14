{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( importifyCacheList
       , importifyCacheProject
       ) where

import           Universum

import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString.Lazy            as LBS (writeFile)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (notElem)

import           Distribution.PackageDescription (BuildInfo (includeDirs),
                                                  GenericPackageDescription)
import           Fmt                             (Builder, blockListF, build, fmt, fmtLn,
                                                  indent, listF, (+|), (+||), (|+), (||+))
import           Language.Haskell.Exts           (Module, ModuleName (..), SrcSpanInfo)
import           Language.Haskell.Names          (writeSymbols)
import           Lens.Micro.Platform             (to)
import           Path                            (Abs, Dir, File, Path, fromAbsDir,
                                                  fromAbsFile, parseAbsFile, parseRelDir,
                                                  parseRelFile, (</>))
import           Path.IO                         (doesDirExist, ensureDir, removeDirRecur)
import           Turtle                          (shell)

import           Extended.System.Wlog            (printDebug, printInfo, printWarning)
import           Importify.Cabal                 (ModulesBundle (..), ModulesMap,
                                                  TargetId (LibraryId),
                                                  buildInfoExtensions,
                                                  extractTargetBuildInfo,
                                                  extractTargetsMap, packageDependencies,
                                                  packageExtensions, packageTargets,
                                                  readCabal, targetIdDir,
                                                  withHarmlessExtensions)
import           Importify.Environment           (CacheEnvironment, HasGhcIncludeDir,
                                                  HasPathToImportify, RIO, ghcIncludeDir,
                                                  pathToImportify, pathToSymbols,
                                                  saveSources)
import           Importify.ParseException        (ModuleParseException, reportErrorsIfAny,
                                                  setMpeFile)
import           Importify.Path                  (decodeFileOrMempty, doInsideDir,
                                                  extensionsPath, findCabalFile,
                                                  modulesFile, modulesPath, symbolsPath)
import           Importify.Preprocessor          (parseModuleWithPreprocessor)
import           Importify.Resolution            (resolveModules)
import           Importify.Stack                 (LocalPackages (..), QueryPackage (..),
                                                  RemotePackages (..), pkgName,
                                                  stackListDependencies,
                                                  stackListPackages, upgradeWithVersions)
import           Importify.Syntax                (getModuleTitle)

-- | This function takes list of explicitly specified dependencies
-- with versions and caches only them under @.importify@ folder inside
-- current directory ignoring .cabal file for project. This function
-- doesn't update mapping from module paths.
importifyCacheList :: NonEmpty Text -> RIO CacheEnvironment ()
importifyCacheList explicitDependencies = do
    printInfo "Using explicitly specified list of dependencies for caching..."
    importifyPath <- view pathToImportify
    doInsideDir importifyPath $
        () <$ cacheDependenciesWith identity
                                    unpackCacher
                                    (toList explicitDependencies)

-- | Caches packages information into local .importify directory by
-- reading this information from @<package-name>.cabal@ file.
importifyCacheProject :: RIO CacheEnvironment ()
importifyCacheProject = do
    (localPackages@(LocalPackages locals), remotePackages) <- stackListPackages
    if null locals
    then printWarning "No packages found :( This could happen due to next reasons:\n\
                      \    1. Not running from project root directory.\n\
                      \    2. 'stack query' command failure.\n\
                      \    3. Our failure in parsing 'stack query' output."
    else cacheProject localPackages remotePackages

cacheProject :: LocalPackages -> RemotePackages -> RIO CacheEnvironment ()
cacheProject (LocalPackages locals) (RemotePackages remotes) = do
    localDescriptions   <- mapM localPackageDescription locals
    hackageDependencies <- extractHackageDependencies localDescriptions
                                                      (locals ++ remotes)

    importifyPath <- view pathToImportify
    doInsideDir importifyPath $ do
        -- 1. Unpack hackage dependencies then cache them
        printInfo $ "Caching total "+|length hackageDependencies|+
                    " dependencies from Hackage: "+|listF hackageDependencies|+""
        hackageMaps <- cacheDependenciesWith identity
                                             unpackCacher
                                             hackageDependencies

        -- 2. Unpack all remote non-hackage dependencies (e.g. from GitHub)
        remoteMaps <- cacheDependenciesWith pkgName
                                            remoteCacher
                                            remotes

        -- 3. Unpack finally all local packages
        localMaps <- forM locals $ \localPackage -> do
            printInfo $ "Caching package: " <> pkgName localPackage
            cachePackage (qpPath localPackage)
                         (pkgName localPackage)
                         True

        updateModulesMap $ (HM.unions localMaps)
                `HM.union` (HM.unions remoteMaps)
                `HM.union` (HM.unions hackageMaps)

localPackageDescription :: MonadIO m => QueryPackage -> m GenericPackageDescription
localPackageDescription QueryPackage{..} = do
    Just cabalPath <- findCabalFile qpPath
    let cabalFile   = fromAbsFile cabalPath
    readCabal cabalFile

extractHackageDependencies :: MonadIO m
                           => [GenericPackageDescription]
                           -> [QueryPackage]
                           -> m [Text]
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
cacheDependenciesWith :: forall d env .
                         (d -> Text)
                      -- ^ How to get dependency name?
                      -> (d -> RIO env ModulesMap)
                      -- ^ How to cache dependency (unpack for StackDependency)
                      -> [d]
                      -- ^ List of dependencies that should be cached
                      -> RIO env [ModulesMap]
cacheDependenciesWith dependencyName dependencyResolver = go
  where
    go :: [d] -> RIO env [ModulesMap]
    go []     = return []
    go (d:ds) = do
        let depName = dependencyName d
        isAlreadyCached depName >>= \case
            True  -> printDebug (depName|+" is already cached") *> go ds
            False -> liftM2 (:) (dependencyResolver d) (go ds)

    isAlreadyCached :: Text -> RIO env Bool
    isAlreadyCached libName = do
        libraryPath       <- parseRelDir $ toString libName
        let libSymbolsPath = symbolsPath </> libraryPath
        doesDirExist libSymbolsPath

-- | This function is passed to 'cacheDependenciesWith' for Hackage dependencies.
unpackCacher :: Text -> RIO CacheEnvironment ModulesMap
unpackCacher libName = do
    _exitCode <- shell ("stack unpack " <> libName) empty

    packagePath         <- parseRelDir $ toString libName
    unpackedPackagePath <- view $ pathToImportify.to (</> packagePath)
    packageModules      <- cachePackage unpackedPackagePath libName False

    -- TODO: use bracket here
    unlessM (view saveSources) $ removeDirRecur packagePath

    pure packageModules

-- | This function is passed to 'cacheDependenciesWith' for 'RemotePackages'.
remoteCacher :: (HasPathToImportify env, HasGhcIncludeDir env)
             => QueryPackage
             -> RIO env ModulesMap
remoteCacher package = do
    let packageName = pkgName package
    printInfo $ "Caching remote package: " <> packageName
    cachePackage (qpPath package) packageName False

-- | Find .cabal file by given path to package and then collect 'ModulesMap'.
cachePackage :: (HasPathToImportify env, HasGhcIncludeDir env)
             => Path Abs Dir  -- ^ Path to package
             -> Text          -- ^ Package name
             -> Bool          -- ^ Is package itself is caching
             -> RIO env ModulesMap
cachePackage packagePath libName isWorkingProject = do
    -- finding path to unpacked package .cabal file
    mCabalFileName   <- findCabalFile packagePath
    let cabalFileName = fromMaybe (error $ "No .cabal file inside: " <> libName)
                                  mCabalFileName

    packageCabalDesc <- readCabal $ fromAbsFile cabalFileName
    createPackageCache packageCabalDesc
                       packagePath
                       libName
                       isWorkingProject

-- | Creates @./.impority/symbols/<package> folder where all symbols
-- for given library stored. This function used for both library packages
-- and project package itself.
createPackageCache :: (HasPathToImportify env, HasGhcIncludeDir env)
                   => GenericPackageDescription
                   -- ^ Package descriptions
                   -> Path Abs Dir
                   -- ^ Path to package root
                   -> Text
                   -- ^ Package name with version
                   -> Bool
                   -- ^ True if project itself is caching now
                   -> RIO env ModulesMap
createPackageCache
    packageCabalDesc
    packagePath
    packageName
    isWorkingProject
  = do
    -- Creates ./.importify/symbols/<package>/ directory
    packageNamePath  <- parseRelDir (toString packageName)
    packageCachePath <- view $ pathToSymbols.to (</> packageNamePath)
    ensureDir packageCachePath

    -- Maps from full path to target and from target to list of extensions
    targetsMap <- liftIO $ extractTargetsMap packagePath packageCabalDesc

    let targetIds = if isWorkingProject
                    then packageTargets packageCabalDesc
                    else [LibraryId]

    -- Cache and store extensions only for working project inside package directory
    when isWorkingProject $ do
        let extensionsMap    = packageExtensions targetIds packageCabalDesc
        let pathToExtensions = packageCachePath </> extensionsPath
        liftIO $ LBS.writeFile (fromAbsFile pathToExtensions)
               $ encodePretty extensionsMap

    let moduleToTargetPairs = HM.toList targetsMap
    concatForM targetIds $ \targetId -> do
        let thisTargetModules = map fst
                              $ filter ((== targetId) . snd) moduleToTargetPairs
        targetPaths <- mapM parseAbsFile thisTargetModules

        -- TODO: implement Buildable for targetId
        let targetInfo = fromMaybe (error $ "No such target: "+||targetId||+"")
                       $ extractTargetBuildInfo targetId packageCabalDesc

        (errors, targetModules) <- parseTargetModules packagePath
                                                      targetPaths
                                                      targetInfo
        let targetDirectory = targetIdDir targetId
        liftIO $ reportErrorsIfAny errors (packageName <> ":" <> targetDirectory)

        targetPath           <- parseRelDir $ toString targetDirectory
        let packageTargetPath = packageCachePath </> targetPath
        ensureDir packageTargetPath

        let moduleToPathMap = HM.fromList $ map (first getModuleTitle) targetModules
        let resolvedModules = resolveModules $ map fst targetModules
        fmap HM.fromList $ forM resolvedModules $ \( ModuleName () moduleTitle
                                                   , resolvedSymbols) -> do
            modSymbolsPath     <- parseRelFile $ moduleTitle ++ ".symbols"
            let moduleCachePath = packageTargetPath </> modSymbolsPath

            -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
            liftIO $ writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

            let modulePath = fromMaybe (error $ "Unknown module: "+|moduleTitle|+"")
                           $ HM.lookup moduleTitle moduleToPathMap
            let bundle     = ModulesBundle packageName moduleTitle targetId
            pure (fromAbsFile modulePath, bundle)

parseTargetModules :: HasGhcIncludeDir env
                   => Path Abs Dir    -- ^ Path like @~/.../.importify/containers-0.5@
                   -> [Path Abs File] -- ^ Paths to modules
                   -> BuildInfo       -- ^ BuildInfo of current target
                   -> RIO env ( [ModuleParseException]
                              , [(Module SrcSpanInfo, Path Abs File)]
                              )
parseTargetModules packagePath pathsToModules targetInfo = do
    -- get include directories for cpphs
    includeDirPaths   <- mapM parseRelDir $ includeDirs targetInfo
    let pkgIncludeDirs = map (fromAbsDir . (packagePath </>)) includeDirPaths

    ghcDir <- view ghcIncludeDir
    let includeDirs = pkgIncludeDirs ++ (toList $ fmap fromAbsDir ghcDir)
    let extensions  = withHarmlessExtensions $ buildInfoExtensions targetInfo

    let moduleParser path = do
            parseRes <- liftIO $
              parseModuleWithPreprocessor extensions
                                          includeDirs
                                          path
            return $ bimap (setMpeFile $ fromAbsFile path)  -- Update error
                           (, path)                         -- Update result
                           parseRes

    partitionEithers <$> mapM moduleParser pathsToModules

updateModulesMap :: MonadIO m => ModulesMap -> m ()
updateModulesMap newCachedModules = do
    existingImportsMap <- decodeFileOrMempty modulesPath return
    let mergedMaps      = newCachedModules `HM.union` existingImportsMap
    liftIO $ LBS.writeFile modulesFile $ encodePretty mergedMaps

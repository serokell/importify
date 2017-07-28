-- | Contains implementation of @importify cache@ command.

module Importify.Main.Cache
       ( doCache
       ) where

import           Universum

import           Data.Aeson                      (decodeStrict)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import qualified Data.ByteString                 as BS (readFile)
import qualified Data.ByteString.Lazy            as LBS (writeFile)
import qualified Data.HashMap.Strict             as HM
import qualified Data.Map                        as M
import           Data.Version                    (showVersion)

import           Distribution.Package            (PackageIdentifier (..))
import           Distribution.PackageDescription (BuildInfo (includeDirs), GenericPackageDescription (packageDescription),
                                                  Library (..),
                                                  PackageDescription (package))
import           Fmt                             (Builder, blockListF, build, fmt, fmtLn,
                                                  indent, listF)
import           Language.Haskell.Exts           (Extension, Module, ModuleName (..),
                                                  SrcSpanInfo)
import           Language.Haskell.Names          (writeSymbols)
import           Path                            (Abs, Dir, File, Path, Rel, fromAbsDir,
                                                  fromAbsFile, fromRelDir, fromRelFile,
                                                  parseAbsDir, parseAbsFile, parseRelDir,
                                                  parseRelFile, (</>))
import           System.Directory                (createDirectoryIfMissing,
                                                  doesDirectoryExist, doesFileExist,
                                                  getCurrentDirectory,
                                                  removeDirectoryRecursive)
import           System.FilePath                 (dropExtension)
import           Turtle                          (shell)

import           Extended.Data.List              (mapMaybeM)
import           Extended.System.Wlog            (printDebug, printInfo, printWarning)
import           Importify.Cabal                 (TargetId (LibraryId),
                                                  buildInfoExtensions,
                                                  extractTargetBuildInfo, getMapBundle,
                                                  modulePaths, packageDependencies,
                                                  packageTargets, readCabal,
                                                  splitOnExposedAndOther, targetIdDir,
                                                  withHarmlessExtensions)
import           Importify.ParseException        (ModuleParseException, reportErrorsIfAny)
import           Importify.Paths                 (cachePath, doInsideDir, extensionsFile,
                                                  findCabalFile, getCurrentPath,
                                                  modulesFile, symbolsPath, targetsFile)
import           Importify.Preprocessor          (parseModuleWithPreprocessor)
import           Importify.Resolution            (resolveModules)
import           Importify.Stack                 (ghcIncludePath, stackListDependencies,
                                                  upgradeWithVersions)
import           Importify.Syntax                (debugAST)

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

        -- Libraries
        libVersions    <- stackListDependencies
        let projectName = dropExtension $ fromRelFile cabalFile
        let libraries   = sort
                        $ upgradeWithVersions libVersions
                        $ filter (/= projectName)
                        $ packageDependencies projectCabalDesc
        printInfo $ fmt $ "Caching next dependencies: " <> listF libraries

        -- download & unpack sources, then cache and delete
        dependenciesResolutionMaps <- unpackAndCacheDependencies importifyPath
                                                                 preserveSources
                                                                 libraries

        let PackageIdentifier{..} = package $ packageDescription projectCabalDesc
        let fullProjectName       = toText $ projectName ++ '-':showVersion pkgVersion
        projectResolutionMap <- createProjectCache projectCabalDesc
                                                   projectPath
                                                   (importifyPath </> symbolsPath)
                                                   (packageTargets projectCabalDesc)
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
                   -> IO (Map String Text)
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
    (targetMap, extensionMap) <- getMapBundle packagePath packageCabalDesc

    -- Cache MapBundle only for working project
    when isWorkingProject $ do
        LBS.writeFile targetsFile    $ encodePretty targetMap
        LBS.writeFile extensionsFile $ encodePretty extensionMap

    -- [IMRF-70]: move this to ReaderT config
    ghcIncludeDir  <- runMaybeT ghcIncludePath

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

        let resolvedModules = resolveModules targetModules
        fmap M.fromList $ forM resolvedModules $ \( ModuleName () moduleTitle
                                                  , resolvedSymbols) -> do
            modSymbolsPath     <- parseRelFile $ moduleTitle ++ ".symbols"
            let moduleCachePath = packageTargetPath </> modSymbolsPath

            -- creates ./.importify/symbols/<package>/<Module.Name>.symbols
            writeSymbols (fromAbsFile moduleCachePath) resolvedSymbols

            pure (moduleTitle, packageName)

parseTargetModules :: Path Abs Dir    -- ^ Path like @~/.../.importify/containers-0.5@
                   -> [Path Abs File] -- ^ Paths to modules
                   -> BuildInfo       -- ^ BuildInfo of current target
                   -> Maybe FilePath  -- ^ Path to ghc bundled headers
                   -> IO ([ModuleParseException], [Module SrcSpanInfo])
parseTargetModules packagePath pathsToModules targetInfo ghcIncludeDir = do
    -- get include directories for cpphs
    includeDirPaths   <- mapM parseRelDir $ includeDirs targetInfo
    let pkgIncludeDirs = map (fromAbsDir . (packagePath </>)) includeDirPaths

    let includeDirs  = pkgIncludeDirs ++ toList ghcIncludeDir
    let extensions   = withHarmlessExtensions $ buildInfoExtensions targetInfo
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
    LBS.writeFile modulesFile $ encodePretty mergedMaps

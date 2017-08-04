{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Utilities which allows to use @stack@ tools for different
-- dependencies stuff.

module Importify.Stack
       ( QueryPackage   (..)
       , LocalPackages  (..)
       , RemotePackages (..)

       , ghcIncludePath
       , pkgName
       , stackListDependencies
       , stackListPackages
       , upgradeWithVersions
       ) where

import           Universum

import qualified Control.Foldl        as Fold (head, list)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (notElem, partition)
import           Data.Yaml            (FromJSON (parseJSON), Parser, Value (Object),
                                       decodeEither', prettyPrintParseException,
                                       withObject, (.:))
import           Path                 (Abs, Dir, Path, PathException, dirname, fromAbsDir,
                                       mkRelDir, parent, parseAbsDir, (</>))
import           System.Directory     (doesDirectoryExist)
import           System.FilePath      (splitPath)
import           Turtle               (Line, Shell, inproc, lineToText, linesToText)
import qualified Turtle               (fold)

import           Extended.System.Wlog (printWarning)

shStack :: [Text] -> Shell Line
shStack args = inproc "stack" args empty

pathArgs, depsArgs :: [Text]
pathArgs = ["path", "--compiler-bin"]
depsArgs = ["list-dependencies", "--test", "--bench"]

-- | This function finds path to directory where @include@ for ghc lies.
-- Filepath looks like this:
-- @
--   ~/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/include
-- @
-- This function needed to tell dependencies about files like @"MachDeps.h"@.
--
-- TODO: use GHC path from project?
ghcIncludePath :: MaybeT IO FilePath
ghcIncludePath = do
    ghcBinLine <- MaybeT $ Turtle.fold (shStack pathArgs) Fold.head

    -- ghcBinText ≡ /home/user/.stack/programs/x86_64-linux/ghc-8.0.2/bin
    ghcBinText    <- parseAbsDir $ toString $ lineToText ghcBinLine
    let ghcProgram = parent ghcBinText   -- w/o bin
    let ghcName    = dirname ghcProgram  -- ≡ ghc-8.0.2

    -- ghcInclude ≡ /home/user/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/include
    let ghcInclude = ghcProgram
                 </> $(mkRelDir "lib/")
                 </> ghcName
                 </> $(mkRelDir "include/")

    let ghcIncludeDir = fromAbsDir ghcInclude
    guardM $ liftIO $ doesDirectoryExist ghcIncludeDir
    return ghcIncludeDir

-- TODO: remove after universum update to 0.6
guardM :: MonadPlus m => m Bool -> m ()
guardM f = guard =<< f

-- | Extract all dependencies with versions using
-- @stack list-dependencies@ shell command.
stackListDependencies :: IO (HashMap Text Text)
stackListDependencies = do
    dependencies   <- Turtle.fold (shStack depsArgs) Fold.list
    let wordifyDeps = map (words . lineToText) dependencies
    let pairifyDeps = pairifyList wordifyDeps
    return $ HM.fromList pairifyDeps
  where
    pairifyList :: [[a]] -> [(a,a)]
    pairifyList        []  = []
    pairifyList ([x,y]:xs) = (x,y) : pairifyList xs
    pairifyList     (_:xs) =         pairifyList xs

-- | Takes mapping from package names to their versions and list of
-- packages adding version to each package which is inside dictionary.
upgradeWithVersions :: HashMap Text Text -> [Text] -> [Text]
upgradeWithVersions versions = go
  where
    go        []  = []
    go (lib:libs) = case HM.lookup lib versions of
        Nothing      -> lib                   : go libs
        Just version -> lib <> "-" <> version : go libs

-- | Queries list of all local packages for project. If some errors
-- occur then warning is printed into console and empty list returned.
stackListPackages :: IO (LocalPackages, RemotePackages)
stackListPackages = do
    pkgsYaml    <- linesToText <$> Turtle.fold (shStack ["query"]) Fold.list
    let parseRes = decodeEither' $ encodeUtf8 pkgsYaml
    case parseRes of
        Left exception -> do
            printWarning $ toText $ prettyPrintParseException exception
            return mempty
        Right (StackQueryResult packages) -> do
            localPackages <- mapM toPackage packages `catch` parseErrorHandler
            let (locals, remotes) = partition (isLocalPackage . qpPath) localPackages
            return (LocalPackages locals, RemotePackages remotes)
  where
    toPackage :: (Text, (FilePath, Text)) -> IO QueryPackage
    toPackage (qpName, (path, qpVersion)) = do
        qpPath <- parseAbsDir path
        return QueryPackage{..}

    parseErrorHandler :: PathException -> IO [QueryPackage]
    parseErrorHandler exception =
        [] <$ printWarning ("'stack query' exception: " <> show exception)

    isLocalPackage :: Path Abs Dir -> Bool
    isLocalPackage = notElem ".stack-work/" . splitPath . fromAbsDir

-- | This data type represents package returned by @stack query@ command.
data QueryPackage = QueryPackage
    { qpName    :: Text          -- ^ @importify@
    , qpPath    :: Path Abs Dir  -- ^ @\/home\/user\/importify\/@
    , qpVersion :: Text          -- ^ 1.0
    } deriving (Eq, Show)

-- | Show full name of 'QueryPackage' with version.
pkgName :: QueryPackage -> Text
pkgName QueryPackage{..} = qpName <> "-" <> qpVersion

-- | Local subpackages from exactly this project.
newtype LocalPackages = LocalPackages [QueryPackage]
    deriving (Eq, Monoid)

-- | Remote packages, from GitHub or other locations.
newtype RemotePackages = RemotePackages [QueryPackage]
    deriving (Eq, Monoid)

newtype StackQueryResult = StackQueryResult [(Text, (FilePath, Text))]
    deriving Show

instance FromJSON StackQueryResult where
    parseJSON = withObject "stack query" $ \obj -> do
        Just (Object locals) <- pure $ HM.lookup "locals" obj
        packages <- forM locals $ withObject "package" $ \pkgObj -> do
            pkgPath    :: FilePath <- pkgObj .: "path"
            pkgVersion :: Text     <- pkgObj .: "version"
            pure (pkgPath, pkgVersion)
        pure $ StackQueryResult $ HM.toList packages

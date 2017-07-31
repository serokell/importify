{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Utilities which allows to use @stack@ tools for different
-- dependencies stuff.

module Importify.Stack
       ( LocalPackage (..)
       , ghcIncludePath
       , pkgName
       , stackListDependencies
       , stackListPackages
       , upgradeWithVersions
       ) where

import           Universum

import qualified Control.Foldl        as Fold (head, list)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (notElem)
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
import           Importify.Syntax     (debugAST)

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
stackListDependencies :: IO (HashMap String String)
stackListDependencies = do
    dependencies <- Turtle.fold (shStack depsArgs) Fold.list
    let wordifyDeps = map (map toString . words . lineToText) dependencies
    let pairifyDeps = pairifyList wordifyDeps
    return $ HM.fromList pairifyDeps
  where
    pairifyList :: [[a]] -> [(a,a)]
    pairifyList        []  = []
    pairifyList ([x,y]:xs) = (x,y) : pairifyList xs
    pairifyList     (_:xs) =         pairifyList xs

-- | Takes mapping from package names to their versions and list of
-- packages adding version to each package which is inside dictionary.
upgradeWithVersions :: HashMap String String -> [String] -> [String]
upgradeWithVersions versions = go
  where
    go        []  = []
    go (lib:libs) = case HM.lookup lib versions of
        Nothing      -> lib                   : go libs
        Just version -> lib <> "-" <> version : go libs

-- | Queries list of all local packages for project. If some errors
-- occur then warning is printed into console and empty list returned.
stackListPackages :: IO [LocalPackage]
stackListPackages = do
    pkgsYaml    <- linesToText <$> Turtle.fold (shStack ["query"]) Fold.list
    let parseRes = decodeEither' $ encodeUtf8 pkgsYaml
    case parseRes of
        Left exception -> do
            printWarning $ toText $ prettyPrintParseException exception
            return []
        Right (StackQuery packages) -> do
            localPackages <- mapM toPackage packages `catch` parseErrorHandler
            let projectPackages = filter (isLocalPackage . lpPath) localPackages
            return projectPackages
  where
    toPackage :: (Text, (FilePath, Text)) -> IO LocalPackage
    toPackage (lpName, (path, lpVersion)) = do
        lpPath <- parseAbsDir path
        return LocalPackage{..}

    parseErrorHandler :: PathException -> IO [LocalPackage]
    parseErrorHandler exception = do
        printWarning $ "'stack query' exception: " <> show exception
        return []

    isLocalPackage :: Path Abs Dir -> Bool
    isLocalPackage = notElem ".stack-work/" . splitPath . fromAbsDir

-- | This data type represents package returned by @stack query@ command.
data LocalPackage = LocalPackage
    { lpName    :: Text          -- ^ @importify@
    , lpPath    :: Path Abs Dir  -- ^ @\/home\/user\/importify\/@
    , lpVersion :: Text          -- ^ 1.0
    } deriving (Show)

-- | Show full name of 'LocalPackage' with version.
pkgName :: LocalPackage -> Text
pkgName LocalPackage{..} = lpName <> "-" <> lpVersion

newtype StackQuery = StackQuery [(Text, (FilePath, Text))]
    deriving Show

instance FromJSON StackQuery where
    parseJSON = withObject "stack query" $ \obj -> do
        Just (Object locals) <- pure $ HM.lookup "locals" obj
        packages <- forM locals $ withObject "package" $ \pkgObj -> do
            pkgPath    :: FilePath <- pkgObj .: "path"
            pkgVersion :: Text     <- pkgObj .: "version"
            pure (pkgPath, pkgVersion)
        pure $ StackQuery $ HM.toList packages

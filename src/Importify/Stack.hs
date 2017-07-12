{-# LANGUAGE TemplateHaskell #-}

-- | Utilities which allows to use @stack@ tools for different
-- dependencies stuff.

module Importify.Stack
       ( ghcIncludePath
       , stackListDependencies
       , upgradeWithVersions
       ) where

import           Universum

import qualified Control.Foldl       as Fold (head, list)
import qualified Data.HashMap.Strict as HM
import           Path                (dirname, fromAbsDir, mkRelDir, parent, parseAbsDir,
                                      (</>))
import           System.Directory    (doesDirectoryExist)
import           Turtle              (Line, Shell, inproc, lineToText)
import qualified Turtle              (fold)

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

    -- ghcBinText looks like /home/user/.stack/programs/x86_64-linux/ghc-8.0.2/bin
    ghcBinText    <- parseAbsDir $ toString $ lineToText ghcBinLine
    let ghcProgram = parent ghcBinText   -- w/o bin
    let ghcName    = dirname ghcProgram  -- ≡ ghc-8.0.2
    let ghcInclude = ghcProgram
                 </> $(mkRelDir "lib/")
                 </> ghcName
                 </> $(mkRelDir "include/")

    let ghcIncludeDir = fromAbsDir ghcInclude
    guardM $ liftIO $ doesDirectoryExist ghcIncludeDir
    return ghcIncludeDir

-- TODO: remove after universum update
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

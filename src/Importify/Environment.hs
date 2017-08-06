{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | This module contains enrinment for @importify cache@ command
-- which is running inside @ReaderT env IO@ monad.

module Importify.Environment
       ( -- * Base monad for @importify cache@ command
         RIO (..)

         -- * Environment for @importify cache@ command
       , CacheEnvironment (..)
       , HasGhcIncludeDir
       , HasPathToImportify
       , HasSaveSources
       , ghcIncludeDir
       , pathToImportify
       , pathToSymbols
       , saveSources

         -- * Runner for cache commend
       , runCache
       ) where

import           Universum

import           Lens.Micro       (SimpleGetter, to)
import           Lens.Micro.TH    (makeLensesWith)
import           Path             (Abs, Dir, Path, (</>))

import           Extended.Lens.TH (fieldsVerboseLensRules)
import           Importify.Path   (getCurrentPath, importifyPath, symbolsPath)
import           Importify.Stack  (ghcIncludePath)

-- | 'ReaderT' + 'IO' monad described here:
-- https://www.fpcomplete.com/blog/2017/07/the-rio-monad
newtype RIO env a = RIO
    { runRIO :: ReaderT env IO a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadReader env, MonadThrow, MonadCatch, MonadMask)

-- | Environment for @importify cache@ command.
data CacheEnvironment = CacheEnvironment
    { -- | Path to local @.importify@ folder
      _pathToImportify :: !(Path Abs Dir)

      -- | Path to GHC .h files
    , _ghcIncludeDir   :: !(Maybe FilePath)

      -- | 'True' if unpacked sources should be stored locally as well
    , _saveSources     :: !Bool
    }

makeLensesWith fieldsVerboseLensRules ''CacheEnvironment

type HasPathToImportify env = HasPolyPathToImportify env (Path Abs Dir)
type HasGhcIncludeDir   env = HasPolyGhcIncludeDir   env (Maybe FilePath)
type HasSaveSources     env = HasPolySaveSources     env Bool

-- | Getter of @~\/path\/to\/project\/.importify\/symbols@ folder.
pathToSymbols :: HasPathToImportify env => SimpleGetter env (Path Abs Dir)
pathToSymbols = pathToImportify.to (</> symbolsPath)

-- | Run @importify cache@ command. This function takes current
-- project directory and searches for ghc include path.
runCache :: Bool -> RIO CacheEnvironment () -> IO ()
runCache _saveSources cacheAction = do
    projectPath         <- getCurrentPath
    let _pathToImportify = projectPath </> importifyPath
    _ghcIncludeDir      <- runMaybeT ghcIncludePath
    usingReaderT CacheEnvironment{..} $ runRIO cacheAction

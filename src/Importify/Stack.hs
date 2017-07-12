{-# LANGUAGE TemplateHaskell #-}

-- | Utilities which allows to use @stack@ tools for different
-- dependencies stuff.

module Importify.Stack
       ( ghcIncludePath
       ) where

import           Universum

import qualified Control.Foldl    as Fold (head)
import           Path             (dirname, fromAbsDir, mkRelDir, parent, parseAbsDir,
                                   (</>))
import           System.Directory (doesDirectoryExist)
import           Turtle           (inproc, lineToText)
import qualified Turtle           (fold)

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
    ghcBinLine <- MaybeT
                $ Turtle.fold (inproc "stack" ["path", "--compiler-bin"] empty)
                              Fold.head

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

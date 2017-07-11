-- | Functions that work with 'Extension's in .cabal file and in
-- separate.

module Importify.Cabal.Extension
       ( libraryExtensions
       , withHarmlessExtensions
       ) where

import           Universum

import qualified Data.HashMap.Strict                   as Map
import           Data.List                             (partition)
import           Distribution.ModuleName               (ModuleName, fromString,
                                                        toFilePath)
import qualified Distribution.ModuleName               as Cabal
import           Distribution.Package                  (Dependency (..), PackageName (..))
import           Distribution.PackageDescription       (Benchmark (benchmarkBuildInfo),
                                                        BuildInfo (..), CondTree,
                                                        Executable (..),
                                                        GenericPackageDescription (..),
                                                        Library (..),
                                                        TestSuite (testBuildInfo),
                                                        condTreeData, exeModules,
                                                        libModules)
import           Distribution.PackageDescription.Parse (readPackageDescription)
import           Distribution.Verbosity                (normal)
import qualified Language.Haskell.Extension            as Cabal (Extension (EnableExtension),
                                                                 KnownExtension (..))
import qualified Language.Haskell.Exts                 as HSE
import           Language.Haskell.Exts.Extension       (Extension (..),
                                                        KnownExtension (..))
import           Path                                  (Abs, Dir, File, Path, Rel,
                                                        fromAbsFile, parseRelDir,
                                                        parseRelFile, (</>))
import           System.Directory                      (doesFileExist)
import           System.FilePath.Posix                 (dropExtension)
import           Text.Read                             (read)

import           Importify.Syntax                      (getModuleTitle)

-- | Get list of all extensions from 'Library' and convert them into
-- 'HSE.Extension'.
libraryExtensions :: Library -> [Extension]
libraryExtensions library =
    let BuildInfo{..} = libBuildInfo library
    in map cabalExtToHseExt
     $ filter isHseExt
     $ defaultExtensions ++ otherExtensions

cabalExtToHseExt :: Cabal.Extension -> Extension
cabalExtToHseExt = {- trace ("Arg = " ++ show ext ++ "") -} read . show

isHseExt :: Cabal.Extension -> Bool
isHseExt (Cabal.EnableExtension Cabal.NegativeLiterals) = False
isHseExt (Cabal.EnableExtension Cabal.Unsafe)           = False
isHseExt _                                              = True

-- | This function add list of harmless extensions wich helps to avoid
-- some parsing errors but doesn't affect already correct parsing
-- results.
withHarmlessExtensions :: [Extension] -> [Extension]
withHarmlessExtensions = ordNub
                       . (++ map EnableExtension [ MultiParamTypeClasses
                                                 , FlexibleContexts
                                                 , ConstraintKinds
                                                 ])

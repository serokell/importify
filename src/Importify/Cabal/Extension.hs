-- | Functions that work with 'Extension's in .cabal file and in
-- separate.

module Importify.Cabal.Extension
       ( buildInfoExtensions
       , showExt
       , withHarmlessExtensions
       ) where

import           Universum

import qualified Distribution.ModuleName         as Cabal
import           Distribution.PackageDescription (BuildInfo (..))
import qualified Language.Haskell.Extension      as Cabal (Extension (..),
                                                           KnownExtension (..))
import           Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..))
import           Text.Read                       (read)


-- | Get list of all extensions from 'BuildInfo' and convert them into
-- 'HSE.Extension'.
buildInfoExtensions :: BuildInfo -> [Extension]
buildInfoExtensions BuildInfo{..} = map cabalExtToHseExt
                                  $ filter isHseExt
                                  $ defaultExtensions ++ otherExtensions

cabalExtToHseExt :: Cabal.Extension -> Extension
cabalExtToHseExt = {- trace ("Arg = " ++ show ext ++ "") -} read . show

isHseExt :: Cabal.Extension -> Bool
isHseExt (Cabal.EnableExtension Cabal.NegativeLiterals) = False
isHseExt (Cabal.EnableExtension Cabal.Unsafe)           = False
isHseExt _                                              = True

showExt :: Cabal.Extension -> String
showExt (Cabal.EnableExtension ext)   = show ext
showExt (Cabal.DisableExtension ext)  = "No" ++ show ext
showExt (Cabal.UnknownExtension name) = name

-- | This function add list of harmless extensions wich helps to avoid
-- some parsing errors but doesn't affect already correct parsing
-- results.
withHarmlessExtensions :: [Extension] -> [Extension]
withHarmlessExtensions = ordNub
                       . (++ map EnableExtension [ MultiParamTypeClasses
                                                 , FlexibleContexts
                                                 , ConstraintKinds
                                                 , ExplicitNamespaces
                                                 ])

-- | Functions that work with 'Extension's in .cabal file and in
-- separate.

module Importify.Cabal.Extension
       ( buildInfoExtensions
       , withHarmlessExtensions
       ) where

import           Universum

--import qualified Distribution.ModuleName         as Cabal
import           Distribution.PackageDescription (BuildInfo (..))
import qualified Language.Haskell.Extension      as Cabal (Extension (..))
import           Language.Haskell.Exts.Extension (Extension (..), KnownExtension (..))


-- | Get list of all extensions from 'BuildInfo' and convert them into
-- 'HSE.Extension'.
buildInfoExtensions :: BuildInfo -> [Extension]
buildInfoExtensions BuildInfo{..} = mapMaybe cabalExtToHseExt
                                  $ defaultExtensions ++ otherExtensions

cabalExtToHseExt :: Cabal.Extension -> Maybe Extension
cabalExtToHseExt = readMaybe . show

{-
showExt :: Cabal.Extension -> String
showExt (Cabal.EnableExtension ext)   = show ext
showExt (Cabal.DisableExtension ext)  = "No" ++ show ext
showExt (Cabal.UnknownExtension name) = name
-}

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

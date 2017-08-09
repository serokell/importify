-- | @TemplateHaskell@ utilities for generating lens fields.

module Extended.Lens.TH
       ( fieldsVerboseLensRules
       ) where

import           Universum

import           Data.Char                  (toUpper)
import           Data.List                  (stripPrefix)
import           Language.Haskell.TH.Syntax (Name, mkName, nameBase)
import           Lens.Micro.Platform        (DefName (MethodName), LensRules,
                                             camelCaseFields, lensField, makeLensesWith)

-- | A field namer for 'fieldsVerboseLensRules'.
verboseFieldsNamer :: Name -> [Name] -> Name -> [DefName]
verboseFieldsNamer _ _ fieldName = maybeToList $ do
    fieldUnprefixed@(x:xs) <- stripPrefix "_" (nameBase fieldName)
    let className  = "HasPoly" ++ toUpper x : xs
    let methodName = fieldUnprefixed
    pure (MethodName (mkName className) (mkName methodName))

-- | Custom rules for generating lenses. This is similar to
-- @makeFields@ but generated type classes have names like @HasPolyFoo@
-- instead of @HasFoo@ so they supposed to be used by introducing new
-- constraint aliases. See 'Importify.Environment' for details.
fieldsVerboseLensRules :: LensRules
fieldsVerboseLensRules = camelCaseFields & lensField .~ verboseFieldsNamer

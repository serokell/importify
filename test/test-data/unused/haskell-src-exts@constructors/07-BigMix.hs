module BigMix where

import Data.Bool (not, otherwise, (&&), (||))
import Data.List (sort, sortOn)
import qualified Data.List                    as L
import           Data.Monoid                  (Monoid)
import Language.Haskell.Exts (Module (Module), ModuleName, parseFile)
import qualified Language.Haskell.Exts        as HSE

import Language.Haskell.Exts.Syntax (Assoc, Decl (ForExp, FunBind, TypeSig),
                                     Namespace)

foo :: Language.Haskell.Exts.Module ()
foo = undefined
foo2 = not
foo3 = sortOn
baz = TypeSig

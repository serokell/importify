module BigMix where                                                           -- module BigMix where
                                                                              --
import Data.Bool (not, otherwise, (&&), (||))                                 -- import Data.Bool (not)
import Data.List (sort, sortOn)                                               -- import Data.List (      sortOn)
import qualified Data.List                    as L
import           Data.Monoid                  (Monoid)
import Language.Haskell.Exts (Module (Module), ModuleName, parseFile)         -- import Language.Haskell.Exts (Module         )
import qualified Language.Haskell.Exts        as HSE
                                                                              --
import Language.Haskell.Exts.Syntax (Assoc, Decl (ForExp, FunBind, TypeSig),  -- import Language.Haskell.Exts.Syntax (       Decl (                 TypeSig))
                                     Namespace)
                                                                              --
foo :: Language.Haskell.Exts.Module ()                                        -- foo :: Language.Haskell.Exts.Module ()
foo = undefined                                                               -- foo = undefined
foo2 = not                                                                    -- foo2 = not
foo3 = sortOn                                                                 -- foo3 = sortOn
baz = TypeSig                                                                 -- baz = TypeSig

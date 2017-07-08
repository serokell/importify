module ImportBothUsedQualified where

import           Language.Haskell.Names (symbolName)
import qualified Language.Haskell.Names as N

main = N.symbolName

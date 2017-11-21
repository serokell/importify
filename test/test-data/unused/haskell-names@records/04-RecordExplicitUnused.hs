module RecordExplicitUnused where

import           Language.Haskell.Names (Symbol (Value, symbolModule, symbolName))

foo = Value {symbolModule = undefined}

module RecordExplicitUsed where

import           Language.Haskell.Names (Symbol (Value, symbolModule))

foo = Value {symbolModule = undefined}

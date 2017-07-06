module MultipleOuterEntriesRemoved where      -- module MultipleOuterEntriesRemoved where
                                              --
import Language.Haskell.Exts (Module(Module)  -- import Language.Haskell.Exts (Module(Module)
                             ,parseFile
                             ,readExtensions) -- )
                                              --
foo = Module                                  -- foo = Module

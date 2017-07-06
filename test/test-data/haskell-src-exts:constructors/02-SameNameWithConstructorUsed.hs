module SameNameWithConstructorUsed where        -- module SameNameWithConstructorUsed where
                                                --
import Language.Haskell.Exts (Module (Module))  -- import Language.Haskell.Exts (Module (Module))
                                                --
foo :: Module ()                                -- foo :: Module ()
foo = Module                                    -- foo = Module

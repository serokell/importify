module SameNameOnlyTypeUsed where               -- module SameNameOnlyTypeUsed where
                                                --
import Language.Haskell.Exts (Module (Module))  -- import Language.Haskell.Exts (Module         )
                                                --
foo :: Module ()                                -- foo :: Module ()
foo = undefined                                 -- foo = undefined

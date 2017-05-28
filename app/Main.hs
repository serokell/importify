module Main where

import           Universum

import qualified Data.Map.Strict       as M
import           Language.Haskell.Exts (ImportDecl (..), ImportSpec (..), Module (..),
                                        ParseResult (..), parseFile, prettyPrint)

import           Importify.Common      (Identifier (..), collectImportsList,
                                        removeImportIdentifier)

main :: IO ()
main = do
    [fileName, id] <- getArgs
    ParseOk m@(Module l h p imports d) <- parseFile fileName

    let importsMap      = collectImportsList imports
    let (newImports, _) = removeImportIdentifier (Identifier id) importsMap imports
    let newModule       = Module l h p newImports d

    putText "----------------- // OLD IMPORTS // --------------"
    mapM_ (putStrLn . prettyPrint) imports

    putStrLn $ "-------------- // " ++ fileName ++ " // --------------"
    putStrLn $ prettyPrint newModule

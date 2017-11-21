-- | This module contains functions which wrap @importify@ commands.

module Importify.Bracket
       ( ImportifyArguments     (..)
       , ImportifyFileException (..)
       , ImportifyFunction
       , ImportifyResult
       , importifyAstBracket
       , importifyOptionsBracket
       , importifyPathBracket
       , loadEnvironment
       ) where

import           Universum

import qualified Data.HashMap.Strict          as HM
import qualified Data.Map                     as M

import           Fmt                          (fmt, (+|), (|+))
import           Language.Haskell.Exts        (Comment (..), Extension, ImportDecl,
                                               Module (..), ModuleName (..), SrcSpanInfo,
                                               exactPrint, parseExtension,
                                               parseFileContentsWithComments)
import           Language.Haskell.Exts.Parser (ParseMode (..), defaultParseMode)
import           Language.Haskell.Names       (Environment, loadBase, readSymbols)
import           Path                         (Abs, File, Path, fromAbsFile, fromRelFile,
                                               parseRelDir, parseRelFile, (</>))
import           Path.IO                      (doesDirExist, getCurrentDir)

import           Extended.System.Wlog         (printError, printNotice)
import           Importify.Cabal              (ExtensionsMap, ModulesBundle (..),
                                               ModulesMap, TargetId, targetIdDir)
import           Importify.OutputOptions
import           Importify.ParseException     (eitherParseResult, setMpeFile)
import           Importify.Path               (decodeFileOrMempty, doInsideDir,
                                               extensionsPath, importifyPath,
                                               lookupToRoot, modulesPath, symbolsPath)
import           Importify.Pretty
import           Importify.Syntax

-- | Type that represents exception occurred during @importify@ command.
newtype ImportifyFileException = IFE Text

-- | Convenient type alias for return result of @importify file@ command.
type ImportifyResult = Either ImportifyFileException Text

-- | Bracket for every command which process file.
importifyOptionsBracket :: (Path Abs File -> IO ImportifyResult)
                        -> OutputOptions
                        -> FilePath
                        -> IO ()
importifyOptionsBracket importify options srcFile = do
    srcPath   <- parseRelFile srcFile
    foundRoot <- lookupToRoot (doesDirExist . (</> importifyPath)) srcPath
    case foundRoot of
        Nothing ->
            printError "Directory '.importify' is not found. Either cache for project \
                       \is not created or not running from project directory."
        Just (rootDir, srcFromRootPath) -> do
            curDir          <- getCurrentDir
            importifyResult <- doInsideDir rootDir $ importify $ curDir </> srcFromRootPath
            handleOptions importifyResult
  where
    handleOptions :: Either ImportifyFileException Text -> IO ()
    handleOptions (Left (IFE msg))    = printError msg
    handleOptions (Right modifiedSrc) = printWithOutputOptions options modifiedSrc

-- | All needed data for @importify@ command which processes one file.
data ImportifyArguments = ImportifyArguments
    { importifyArgumentsAst        :: !(Module SrcSpanInfo)
    , importifyArgumentsModulesMap :: !ModulesMap
    , importifyArgumentsSrc        :: !Text
    , importifyArgumentsComments   :: ![Comment]
    } deriving (Show)

-- | Runs given action over parsed AST.
importifyPathBracket :: (ImportifyArguments -> IO ImportifyResult)
                     -> Path Abs File
                     -> IO ImportifyResult
importifyPathBracket importify srcPath = do
    let srcFile = fromAbsFile srcPath

    modulesMap <- readModulesMap
    extensions <- readExtensions srcPath modulesMap

    whenNothing_ (HM.lookup (fromAbsFile srcPath) modulesMap) $
        printNotice $ "File '"+|srcFile|+"' is not cached: new file or caching error"

    src <- readFile srcFile
    let parseResult = eitherParseResult
                    $ parseFileContentsWithComments (defaultParseMode { extensions = extensions })
                    $ toString src

    case parseResult of
        Left exception       -> return $ Left $ IFE $ setMpeFile srcFile exception |+ ""
        Right (ast,comments) -> importify ImportifyArguments
                                    { importifyArgumentsAst        = ast
                                    , importifyArgumentsModulesMap = modulesMap
                                    , importifyArgumentsSrc        = src
                                    , importifyArgumentsComments   = comments
                                    }

-- | Type of functions which processes imports.
type ImportifyFunction = Module SrcSpanInfo           -- ^ Module where symbols should be removed
                      -> Environment                  -- ^ Environment of all cached modules
                      -> [ImportDecl SrcSpanInfo]     -- ^ Imports from module
                      -> IO [ImportDecl SrcSpanInfo]  -- ^ New imports

-- | Process imports in AST with given function.
-- TODO: this function is ugly :( Write it nicer somehow...
importifyAstBracket :: ImportifyFunction
                    -> ImportifyArguments
                    -> IO ImportifyResult
importifyAstBracket importify ImportifyArguments{..}
    | ast@(Module _ _ _ imports _) <- importifyArgumentsAst =
        Right <$> case importSlice imports of
            Nothing           -> return importifyArgumentsSrc
            Just (start, end) -> importifyAst start end ast imports
    | otherwise = return $ Left $ IFE "Module wasn't parsed correctly"
  where
    importifyAst :: Int
                 -> Int
                 -> Module SrcSpanInfo
                 -> [ImportDecl SrcSpanInfo]
                 -> IO Text
    importifyAst start end ast imports = do
        let codeLines        = lines importifyArgumentsSrc
        let (preamble, rest) = splitAt (start - 1) codeLines
        let (impText, decls) = splitAt (end - start + 1) rest

        environment       <- loadEnvironment importifyArgumentsModulesMap
        newImports        <- importify ast environment imports
        let printedImports = printLovelyImports start
                                                end
                                                importifyArgumentsComments
                                                impText
                                                newImports

        return $ unlines preamble
              <> unlines printedImports
              <> unlines decls

-- | Reads 'ModulesMap' from @.importify/modules@.
readModulesMap :: IO ModulesMap
readModulesMap = decodeFileOrMempty (importifyPath </> modulesPath) pure

readExtensions :: Path Abs File -> ModulesMap -> IO [Extension]
readExtensions srcPath modulesMap = do
    case HM.lookup (fromAbsFile srcPath) modulesMap of
        Nothing                -> return []
        Just ModulesBundle{..} -> do
            packagePath <- parseRelDir $ toString mbPackage
            projectPath <- getCurrentDir
            let pathToExtensions = projectPath
                               </> importifyPath
                               </> symbolsPath
                               </> packagePath
                               </> extensionsPath

            let lookupExtensions = fromMaybe [] . getExtensions mbTarget
            decodeFileOrMempty @ExtensionsMap
                               pathToExtensions
                               (return . lookupExtensions)

getExtensions :: TargetId -> ExtensionsMap -> Maybe [Extension]
getExtensions targetId = fmap (map parseExtension) . HM.lookup targetId

loadEnvironment :: ModulesMap -> IO Environment
loadEnvironment modulesMap = do
    baseEnvironment <- loadBase

    let moduleBundles = HM.elems modulesMap
    packages <- forM moduleBundles $ \ModulesBundle{..} -> do
        packagePath     <- parseRelDir  $ toString mbPackage
        symbolsFilePath <- parseRelFile $ mbModule ++ ".symbols"
        targetPath      <- parseRelDir $ toString $ targetIdDir mbTarget
        let pathToSymbols = importifyPath
                        </> symbolsPath
                        </> packagePath
                        </> targetPath
                        </> symbolsFilePath
        moduleSymbols <- readSymbols (fromRelFile pathToSymbols)
        pure (ModuleName () mbModule, moduleSymbols)

    return $ M.union baseEnvironment (M.fromList packages)

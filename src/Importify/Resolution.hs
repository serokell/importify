-- | This module contains functions to work with name resolution.

module Importify.Resolution
       ( collectUnusedSymbols
       , collectUsedQuals
       , resolveModules
       ) where

import           Universum

import           Data.Data                          (Data)
import qualified Data.Map                           as M

import           Language.Haskell.Exts              (ImportDecl (..), ImportSpecList (..),
                                                     Module, ModuleName (..), QName (..),
                                                     SrcSpanInfo)
import           Language.Haskell.Names             (Environment, NameInfo (GlobalSymbol),
                                                     Scoped (Scoped), resolve, symbolName)
import qualified Language.Haskell.Names             as N
import           Language.Haskell.Names.SyntaxUtils (getModuleName, stringToName)

import           Importify.Syntax                   (Identifier (..),
                                                     importSpecToIdentifiers)

symbolByName :: String -> [N.Symbol] -> Maybe N.Symbol
symbolByName name = find ((stringToName name ==) . symbolName)

symbolUsed :: N.Symbol -> [Scoped l] -> Bool
symbolUsed symbol annotations = any used annotations
  where
    used :: Scoped l -> Bool
    -- Constructors are special because the whole type should be considered used
    -- if one of its constructors is used
    used (Scoped (GlobalSymbol global@(N.Constructor smodule _sname stype) _) _) =
        symbol == global ||
        (N.symbolName symbol == stype && N.symbolModule symbol == smodule)
    -- ditto for selectors
    used (Scoped (GlobalSymbol global@(N.Selector smodule _sname stype _scons) _) _) =
        symbol == global ||
        (N.symbolName symbol == stype && N.symbolModule symbol == smodule)
    -- The symbol is used itself
    used (Scoped (GlobalSymbol global _) _) =
        symbol == global
    used _ =
        False

-- | Collect symbols unused in annotations
collectUnusedSymbols :: Environment -> [ImportDecl l] -> [Scoped l] -> [Identifier]
collectUnusedSymbols env decls annotations = do
    ImportDecl{..}     <- decls
    -- TODO: use Maybe monad here
    Just moduleSymbols <- guarded isJust $ M.lookup (() <$ importModule) env
    Just (ImportSpecList _ _ specs) <- guarded isJust importSpecs
    spec <- specs
    id@(Identifier name) <- importSpecToIdentifiers spec
    case symbolByName name moduleSymbols of
        Just symbol -> do
            guard $ not $ symbolUsed symbol annotations
            pure id
        Nothing ->
            -- error $ toText $ "Unknown symbol " ++ name ++ ". Possible causes: Incomplete cache, invalid sources"
            [] -- Just don't touch it

-- | Gather all symbols for given list of 'Module's. In reality those
-- modules represents all /exposed/ and /other/ modules for one package
-- returning only list of symbols for /exposed/ modules.
resolveModules :: (Data l, Eq l) => [Module l] -> [Module l] -> [(ModuleName (), [N.Symbol])]
resolveModules exposedModules otherModules =
    let symbolsEnv     = resolve (exposedModules ++ otherModules) mempty -- TODO: optimize?
        otherCleared   = map ((() <$) . getModuleName) otherModules

        -- remove @otherModules@ from environment because only @exposed@ can be imported
        exposedEnv     = foldl' (flip M.delete) symbolsEnv otherCleared
        exposedSymbols = M.assocs exposedEnv
    in exposedSymbols

-- | Collect list of modules used for fully qualified names.
-- E.g. if it encounters "IO.putStrLn" it should collect ModuleName "IO"
-- Used later to determine whether `as' import needed or not
collectUsedQuals :: [ImportDecl SrcSpanInfo] -> [Scoped SrcSpanInfo] -> [ModuleName SrcSpanInfo]
collectUsedQuals imports annotations = filter (\qual -> any (qualUsed qual) annotations) quals
  where
    quals :: [ModuleName SrcSpanInfo]
    quals = concatMap (maybeToList . importAs) $ filter (isNothing . importSpecs) imports

qualUsed :: ModuleName SrcSpanInfo -> Scoped SrcSpanInfo -> Bool
qualUsed (ModuleName _ name) (Scoped (GlobalSymbol _ (Qual _ (ModuleName _ usedName) _)) _) =
    name == usedName
qualUsed _ _                                                      = False

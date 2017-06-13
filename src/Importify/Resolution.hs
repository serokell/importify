-- | This module contains functions to work with name resolution.

module Importify.Resolution
       ( collectUnusedSymbols
       , collectUsedQuals
       , resolveOneModule
       ) where

import           Universum

import qualified Data.Map                           as M
import           Data.Maybe                         (listToMaybe)

import           Language.Haskell.Exts              (ImportDecl (..), ImportSpecList (..),
                                                     Module, ModuleName (..), QName (..),
                                                     SrcSpanInfo)
import           Language.Haskell.Names             (Environment, NameInfo (GlobalSymbol),
                                                     Scoped (Scoped), resolve, symbolName)
import qualified Language.Haskell.Names             as N
import           Language.Haskell.Names.SyntaxUtils (stringToName)

import           Importify.Common                   (Identifier (..),
                                                     importSpecToIdentifiers)

symbolByName :: String -> [N.Symbol] -> Maybe N.Symbol
symbolByName name =
    listToMaybe . filter ((stringToName name ==) . symbolName)

symbolUsed :: N.Symbol -> [Scoped l] -> Bool
symbolUsed symbol annotations = any used annotations
  where
    used :: Scoped l -> Bool
    used (Scoped (GlobalSymbol global@(N.Constructor smodule _sname stype) _) _) =
        symbol == global ||
        (N.symbolName symbol == stype && N.symbolModule symbol == smodule)
    used (Scoped (GlobalSymbol global@(N.Selector smodule _sname stype _scons) _) _) =
        symbol == global ||
        (N.symbolName symbol == stype && N.symbolModule symbol == smodule)
    used (Scoped (GlobalSymbol global _) _) =
        symbol == global
    used _ =
        False

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

-- | Gather all symbols for given module.
resolveOneModule :: Module l -> [N.Symbol]
resolveOneModule m =
    let clearedModule = () <$ m
        symbolsEnv    = resolve [clearedModule] mempty
        symbols       = concat $ M.elems symbolsEnv
    in symbols

collectUsedQuals :: [ImportDecl SrcSpanInfo] -> [Scoped SrcSpanInfo] -> [ModuleName SrcSpanInfo]
collectUsedQuals imports annotations = filter (\qual -> any (qualUsed qual) annotations) quals
  where
    quals :: [ModuleName SrcSpanInfo]
    quals = concatMap (maybeToList . importAs) $ filter (isNothing . importSpecs) imports

qualUsed :: ModuleName SrcSpanInfo -> Scoped SrcSpanInfo -> Bool
qualUsed (ModuleName _ name) (Scoped (GlobalSymbol _ (Qual _ (ModuleName _ usedName) _)) _) =
    name == usedName
qualUsed _ _                                                      = False


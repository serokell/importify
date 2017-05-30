{-# LANGUAGE PartialTypeSignatures #-}

-- | This module contains functions to work with name resolution.

module Importify.Resolution
       ( collectUnusedSymbols
       ) where

import           Universum

import qualified Data.Map                           as M

import           Language.Haskell.Exts              (ImportDecl (..), ImportSpecList (..))
import           Language.Haskell.Names             (Environment, NameInfo (GlobalSymbol),
                                                     Scoped (Scoped), symbolName)
import qualified Language.Haskell.Names             as N
import           Language.Haskell.Names.SyntaxUtils (stringToName)

import           Importify.Common                   (Identifier (..),
                                                     importSpecToIdentifier)

symbolByName :: String -> [N.Symbol] -> Maybe N.Symbol
symbolByName name symbols = head $ do
    symbol <- symbols
    guard $ symbolName symbol == stringToName name
    pure symbol

symbolUsages :: N.Symbol -> [Scoped l] -> Maybe l
symbolUsages symbol annotations = head $ do
    Scoped (GlobalSymbol globalSymbol _) location <- annotations
    guard (globalSymbol == symbol)
    pure location

collectUnusedSymbols :: Environment -> [ImportDecl l] -> [Scoped l] -> [Identifier]
collectUnusedSymbols env decls annotations = do
    ImportDecl{..}     <- decls
    -- TODO: use Maybe monad here
    Just moduleSymbols <- guarded isJust $ M.lookup (() <$ importModule) env
    Just (ImportSpecList _ _ specs) <- guarded isJust importSpecs
    spec <- specs
    let id@(Identifier name) = importSpecToIdentifier spec
    Just symbol <- guarded isJust $ symbolByName name moduleSymbols
    guard $ isNothing (symbolUsages symbol annotations)
    pure id

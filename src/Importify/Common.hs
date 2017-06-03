{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common utilities for import list processing

module Importify.Common
       ( Identifier (..)
       , collectImportsList
       , getImportModuleName
       , getModuleName
       , importSpecToIdentifiers
       , importSlice
       , removeIdentifiers
       , removeImportIdentifier
       ) where

import           Universum

import           Data.Char             (isSpace)
import qualified Data.List             as L
import qualified Data.List.NonEmpty    as NE
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M

import           Language.Haskell.Exts (CName (..), ImportDecl (..), ImportSpec (..),
                                        ImportSpecList (..), ModuleName, Name (..),
                                        SrcSpan (..), SrcSpanInfo (..), combSpanInfo)

-- | Returns module name for 'ImportDecl' with annotation erased.
getImportModuleName :: ImportDecl l -> ModuleName ()
getImportModuleName ImportDecl{..} = () <$ importModule

-- | Data type that represents function, operator, type or constructor identifier.
newtype Identifier = Identifier { getIdentifier :: String }
    deriving (Show, Eq, Ord)

type ImportsListMap l = Map Identifier (ImportDecl l, ImportSpec l)

nameToIdentifier :: Name l -> Identifier
nameToIdentifier (Ident  _ name) = Identifier name
nameToIdentifier (Symbol _ name) = Identifier name

cnameToIdentifier :: CName l -> Identifier
cnameToIdentifier (VarName _ name) = nameToIdentifier name
cnameToIdentifier (ConName _ name) = nameToIdentifier name

importSpecToIdentifiers :: ImportSpec l -> [Identifier]
importSpecToIdentifiers (IVar _ name)              = [nameToIdentifier name]
importSpecToIdentifiers (IAbs _ _ name)            = [nameToIdentifier name]
importSpecToIdentifiers (IThingAll _ name)         = [nameToIdentifier name]
importSpecToIdentifiers (IThingWith _ name cnames) = nameToIdentifier name:map cnameToIdentifier cnames


-- | Converts list of 'ImportDecl' to 'Map' from 'Identifier' @id@ to be able
-- to find easily corresponding 'ImportDecl' which export list contains @id@.
collectImportsList :: forall l . [ImportDecl l] -> ImportsListMap l
collectImportsList = foldr go mempty
  where
    go :: ImportDecl l -> ImportsListMap l -> ImportsListMap l
    go imp@ImportDecl{..} dict = maybe dict (updateWithImportSpecList dict imp) importSpecs

    updateWithImportSpecList :: ImportsListMap l
                             -> ImportDecl l
                             -> ImportSpecList l
                             -> ImportsListMap l
    updateWithImportSpecList dict _   (ImportSpecList _ True  _) = dict  -- True means is @hiding@
    updateWithImportSpecList dict imp (ImportSpecList _ False l) =
        foldr (\spec -> insertIdentifierList (importSpecToIdentifiers spec) (imp, spec)) dict l
    insertIdentifierList :: [Identifier]
                         -> (ImportDecl l, ImportSpec l)
                         -> ImportsListMap l
                         -> ImportsListMap l
    insertIdentifierList ids imp dict =
        foldr (\id -> M.insert id imp) dict ids

-- | Replace first entry in given list by applying given function
-- if predicate is @True@. After modification drop element from list if second
-- given predicate is also @True@. This function is basically some smart combination
-- of 'map' and 'filter'.
replaceOrDrop :: (a -> Bool) -> (a -> Bool) -> (a -> a) -> [a] -> [a]
replaceOrDrop shouldBeConsidered shouldBeDropped update = go
  where
    go [] = []
    go (x:xs) = if shouldBeConsidered x
                then let newX = update x in
                     if shouldBeDropped newX then xs else newX : xs
                else x : go xs

deleteSpecThing :: Identifier -> ImportSpec l -> Maybe (ImportSpec l)
deleteSpecThing id (IThingWith l name cnames) = case newCnames of
    [] -> Nothing
    _  -> Just $ IThingWith l name newCnames
  where
    newCnames = filter isId cnames

    isId :: CName l -> Bool
    isId = (id ==) . cnameToIdentifier
deleteSpecThing _ _ = error "deleteSpecThing got something else than IThingWith. That shouldn't happen"

specListDelete :: Eq l => Maybe Identifier -> ImportSpec l -> ImportSpecList l -> Maybe (ImportSpecList l)
specListDelete (Just id) spec specList@(ImportSpecList l b specs) = case deleteSpecThing id spec of
    Nothing      -> specListDelete Nothing spec specList
    Just newSpec -> Just (ImportSpecList l b (newSpec:(L.delete spec specs)))
specListDelete Nothing spec (ImportSpecList l b specs) = case L.delete spec specs of
    [] -> Nothing
    sp -> Just $ ImportSpecList l b sp

deleteImportSpec :: Eq l => Maybe Identifier -> ImportSpec l -> ImportDecl l -> ImportDecl l
deleteImportSpec id spec imp = imp {importSpecs = importSpecs imp >>= specListDelete id spec }

-- | Find 'ImportDecl' that contains given entry identifier and remove this entry
-- from that import declaration.
removeImportIdentifier :: Eq l
                       => Identifier
                       -> ImportsListMap l
                       -> [ImportDecl l]
                       -> ([ImportDecl l], ImportsListMap l)
removeImportIdentifier id dict decls = case M.lookup id dict of
    Nothing ->
        (decls, dict)
    Just (decl, spec@(IThingWith _ name _)) ->
        removeUsing decl $ if id == nameToIdentifier name
                           then deleteImportSpec Nothing spec
                           else deleteImportSpec (Just id) spec
    Just (decl, spec) ->
        removeUsing decl (deleteImportSpec Nothing spec)
  where
    eqByModule = (==) `on` getImportModuleName
    removeUsing decl deleteFunc =
        ( replaceOrDrop (eqByModule decl)
                        (isNothing . importSpecs)
                        deleteFunc
                        decls
        , M.delete id dict
        )


-- | Removes list of 'Identifier's by calling 'removeImportIdentifier' and passing
-- new result to the next recursive call.
removeIdentifiers :: Eq l
                  => [Identifier]
                  -> ImportsListMap l
                  -> [ImportDecl l]
                  -> [ImportDecl l]
removeIdentifiers [] _ decls = decls
removeIdentifiers (id:ids) env decls =
    let (newDecls, newEnv) = removeImportIdentifier id env decls
    in removeIdentifiers ids newEnv newDecls

startAndEndLines :: SrcSpanInfo -> (Int, Int)
startAndEndLines (SrcSpanInfo SrcSpan{..} _) = (srcSpanStartLine, srcSpanEndLine)

-- | Returns pair of line numbers — first and last line of import section
-- if any import is in list.
importSlice :: [ImportDecl SrcSpanInfo] -> Maybe (Int, Int)
importSlice []               = Nothing
importSlice [ImportDecl{..}] = Just $ startAndEndLines importAnn
importSlice (x:y:xs)         = Just $ startAndEndLines
                                    $ combSpanInfo (importAnn x) (importAnn $ NE.last (y :| xs))

-- | Returns module name of the source file
-- We can't parse the whole file to get it because default extensions are not available yet
getModuleName :: Text -> String
getModuleName src = case impl of
                        [mn] -> L.init mn
                        []   -> error "File doesn't have `module' declaration"
                        _    -> error "File has multiple `module' declarations"

  where
    impl :: [String]
    impl = do
        tline <- lines src
        let line = toString tline ++ " "
        if isPrefixOf "module " line then do
            modNameWithTail <- maybeToList $ find (isSpace . L.head) $ L.tails line
            maybeToList $ find (isSpace . L.last) $ drop 1 $ L.inits $ drop 1 modNameWithTail
        else
            []


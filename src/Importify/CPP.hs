{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains utilities for dealing with
-- @-XCPP@ extension in files.

module Importify.CPP
       ( parseWithCPP
       , withModuleAST
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Fmt                       (( #| ), (|#))
import           Language.Haskell.Exts     (Module, ParseResult (..), SrcSpanInfo, Comment,
                                            defaultParseMode, prettyPrint)
import qualified Language.Haskell.Exts     as LHE (SrcLoc)
import           Language.Haskell.Exts.CPP (defaultCpphsOptions,
                                            parseFileWithCommentsAndCPP)
import           Path                      (File, Path, Rel, fromRelFile)

data ModuleParseException = MPE !LHE.SrcLoc !String
    deriving (Show)

instance Exception ModuleParseException
instance Buildable ModuleParseException where
    build (MPE loc reason) = ""#|prettyPrint loc|#" because of: "#|reason|#""

-- | Parses file at given path without lines of c-preprocessor.
-- Some additional handling is required because @haskell-src-exts@ can't
-- handle @-XCPP@.
parseWithCPP :: Path Rel File -> IO $ ParseResult $ (Module SrcSpanInfo, [Comment])
parseWithCPP pathToModule = do
    let moduleFile = fromRelFile pathToModule
    parseFileWithCommentsAndCPP defaultCpphsOptions
                                defaultParseMode
                                moduleFile

-- | Perform action with module AST if it parses successfully.
withModuleAST :: Path Rel File -> (Module SrcSpanInfo -> IO ()) -> IO ()
withModuleAST pathToModule action = parseWithCPP pathToModule >>= \case
        ParseOk (moduleAST, _) -> action moduleAST
        ParseFailed loc reason -> putText $ "Ignoring module cache := "#|MPE loc reason|#""

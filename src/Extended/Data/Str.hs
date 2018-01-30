-- | Utilities to work with textual types.

module Extended.Data.Str
       ( -- * Wrapping
         charWrap
       , wordWrap
       ) where

import Universum

import qualified Data.Text as T

-- | Wraps given string into lines non exceeding given length
-- splitting by chars.
charWrap :: Int -> String -> Text
charWrap n = unlines . T.chunksOf n . toText

-- | Wraps given string into lines non exceeding given length
-- splitting by words.
wordWrap :: Int -> String -> Text
wordWrap n = unlines . map unwords . go 0 [] . words . toText
  where
    go :: Int -> [Text] -> [Text] -> [[Text]]
    go _   row    []  = [row]
    go acc row (w:ws) = let wLen   = length w
                            newAcc = acc + wLen
                        in if newAcc > n
                           then row : go wLen [w] ws
                           else go newAcc (row ++ [w]) ws

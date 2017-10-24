module PreserveComments  where

import Data.Bool (Bool(..))
-- keep this
import {- multiple -} qualified {- comments -}   Data.List (replicate) {- on one line! -}
import Data.Text -- also this
{-
   but not this one
-}
import Control.Monad (forM_)
import {- B -} qualified Data.Map {- D -} as {- E -} Map {- F -} (empty)


-- Dummy code
main :: IO ()
main = if True
        then forM_ (Data.List.replicate 2 2) $ return ()
        else let m = Map.empty in return ()

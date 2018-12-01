{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE TupleSections, GADTs #-}

module Day1 () where

import AdventPrelude hiding ()
-- import qualified Data.Grid.Mutable as MG
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

input :: IO Text
input = readFile "input-X.txt"

-- p1 :: IO ()
-- p1 = do
--   ls <- lines <$> input
--   let ds = fmap readInt ls
--   print (sum ds)

-- someParser :: Parser ()
-- someParser = pure ()

-- p1 :: IO ()
-- p1 = do
--   xs <- parse someParser "" <$> input
--   case xs of
--     Right ns -> do
--       print ns
--     Left e -> putStr (parseErrorPretty e)

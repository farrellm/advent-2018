{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day2 () where

import AdventPrelude
import qualified Data.Map as M

input :: IO Text
input = readFile "input-2.txt"

-- freq :: Ord a => [a] -> Map a Int
-- freq cs = foldl' (flip $ M.alter f) M.empty cs
--   where
--     f Nothing = Just 1
--     f (Just x) = Just (x + 1)

p1 :: IO ()
p1 = do
  ls <- lines <$> input
  let cs = fmap (freq . toS) ls :: [Map Char Int]
      x = length $ filter (any (== 2) . M.elems) cs
      y = length $ filter (any (== 3) . M.elems) cs
  print (x * y)

p2 :: IO ()
p2 = do
  ls <- lines <$> input
  let (z : _) = do
        a <- ls
        b <- ls
        let ps = zip (toS a :: [Char]) (toS b)
            ds = filter (uncurry (/=)) ps
        if length ds == 1
          then pure ps
          else []
      r = fmap fst $ filter (uncurry (==)) z
  print r

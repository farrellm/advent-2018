{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day2 () where

import AdventPrelude
import qualified Data.Map as M

input :: IO Text
input = readFile "data/input-2.txt"

-- freq :: Ord a => [a] -> Map a Int
-- freq cs = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty cs

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

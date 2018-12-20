{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day14 (p1, p1', p2) where

import AdventPrelude
import qualified Data.List as L
import qualified Data.Sequence as Q

input :: IO Text
input = readFile "data/input-14.txt"
-- input = readFile "data/test-14.txt"

-- digits :: Int -> [Int]
-- digits i = fmap (subtract (ord '0') . ord) $ show i

next :: Int -> Int -> [Int]
next a b = digits (a + b)

step :: (Int, Int) -> State (Seq Int) ((Int, Int))
step (i, j) = do
  sb <- get
  a <- (`Q.index` i) <$> get
  b <- (`Q.index` j) <$> get
  modify (<> (Q.fromList $ next a b))
  l <- Q.length <$> get
  let i' = (i + a + 1) `mod` l
      j' = (j + b + 1) `mod` l
  pure (i', j')

p1 :: IO ()
p1 = do
  let go x _ = step x
      (x, s) = runState (foldM go (0, 1) [1 .. 556061]) $ Q.fromList [3, 7]
  print (take 10 . drop 556061 $ toList s)

step' :: (Int, Int) -> Seq Int -> [Int]
step' (i, j) sb =
  let a = Q.index sb i
      b = Q.index sb j
      nx = next a b
      sb' = sb <> (Q.fromList $ nx)
      i' = (i + a + 1) `mod` Q.length sb'
      j' = (j + b + 1) `mod` Q.length sb'
  in nx <> step' (i', j') sb'

p1' :: IO ()
p1' = do
  let sb = [3, 7]
      sb' = sb <> step' (0, 1) (Q.fromList sb)
  print (take 10 . drop 556061 $ sb')

p2 :: IO ()
p2 = do
  let sb = [3, 7]
      sb' = sb <> step' (0, 1) (Q.fromList sb)
      k = digits 556061
      l = length k
  print (L.findIndex (== k) . fmap (take l) $ tails sb')

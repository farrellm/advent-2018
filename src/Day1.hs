{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day1 () where

import AdventPrelude
import qualified Data.Set as S

input :: IO Text
input = readFile "input-1.txt"

p1 :: IO ()
p1 = do
  ls <- lines <$> input
  let ds = fmap readInt ls
  print (sum ds)

go :: Set Int -> Int -> Either Int (Set Int)
go s v =
  if v `S.member` s
    then Left v
    else pure (v `S.insert` s)

p2 :: IO ()
p2 = do
  ls <- lines <$> input
  let ds = fmap readInt ls
      ss = scanl' (+) 0 (cycle ds)
      s = foldM go S.empty ss
  print s

p2' :: IO ()
p2' = do
  ls <- lines <$> input
  let ds = fmap readInt ls
      ss = scanl' (+) 0 (cycle ds)
      ps = zip ss (scanl' (flip S.insert) S.empty ss)
      r = fst <$> find (uncurry S.member) ps
  print r

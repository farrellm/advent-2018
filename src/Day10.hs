{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ApplicativeDo #-}

module Day10 (p1) where

import AdventPrelude
import qualified Data.List as L
import qualified Data.Set as S

input :: IO Text
input = readFile "data/input-10.txt"
-- input = readFile "data/test-10.txt"

int' :: Parser Int
int' = int <|> (string " " *> int)

pairs :: Parser ((Int, Int), (Int, Int))
pairs = do
  string "position=<"
  x <- int'
  string ", "
  y <- int'
  string "> velocity=<"
  u <- int'
  string ", "
  v <- int'
  string ">"
  pure ((x, y), (u, v))

inc :: (Num a) => (a, a) -> (a, a) -> (a, a)
inc (x, y) (u, v) = (x + u, y + v)

height :: (Num a, Ord a) => [((a, a), (a, a))] -> a
height ps =
  let ys = fmap (snd . fst) ps
   in maximum ys - minimum ys

valley :: (Ord a) => [a] -> Int
valley zs = go zs 0
  where
    go (x:y:ys) i
      | x < y = i
      | otherwise = go (y : ys) (i + 1)
    go _ i = i

step :: (Num a) => ((a, a), (a, a)) -> ((a, a), (a, a))
step (x, v) = (inc x v, v)

p1 :: IO ()
p1 = do
  ps <- parse (pairs `sepEndBy` eol) "" <$> input
  case ps of
    Right ns -> do
      let nss = iterate (fmap step) ns
          hs = fmap height nss
          i = valley hs
          ms = nss L.!! i
          cs = fmap fst ms
          xs = fmap fst cs
          ys = fmap snd cs
          cs' = S.fromList cs
      for_ [minimum ys .. maximum ys] $ \y -> do
        for_ [minimum xs .. maximum xs] $ \x ->
          if (x, y) `S.member` cs'
           then putStr ("#" :: Text)
           else putStr ("." :: Text)
        putStrLn ("" :: Text)
      print i
    Left e -> putStr (errorBundlePretty e)

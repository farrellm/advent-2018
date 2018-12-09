{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE TupleSections, GADTs, RecordWildCards #-}

module Day6 (p1, p2) where

import AdventPrelude hiding ()
import qualified Data.Map as M
import qualified Data.Set as S

input :: IO Text
input = readFile "data/input-6.txt"
-- input = readFile "data/test-6.txt"

pair :: Parser (Int, Int)
pair = (,) <$> int <* string ", " <*> int

-- dist1 :: Num n => (n, n) -> (n, n) -> n
-- dist1 (a, b) (c, d) = abs (a - c) + abs (b - d)

p1 :: IO ()
p1 = do
  xs <- parse (pair `sepEndBy` eol) "" <$> input
  case xs of
    Right cs -> do
      let x1 = minimum $ fmap fst cs
          x2 = maximum $ fmap fst cs
          y1 = minimum $ fmap snd cs
          y2 = maximum $ fmap snd cs
          edge =
            mconcat
              [ fmap (x1, ) [y1 .. y2]
              , fmap (x2, ) [y1 .. y2]
              , fmap (, y1) [x1 .. x2]
              , fmap (, y2) [x1 .. x2]
              ]
          mid = do
            x <- [x1 + 1 .. x2 - 1]
            y <- [y1 + 1 .. y2 - 1]
            pure (x, y)

          closest p =
            let ds = fmap (dist1 p) cs
                ((d1, k):(d2, _):_) = sort $ zip ds cs
             in if d1 == d2
                  then Nothing
                  else Just k

          inf = S.fromList $ mapMaybe closest edge
          ns = freq $ mapMaybe closest mid
      print (findMaxAssoc $ M.withoutKeys ns inf)
    Left e -> putStr (errorBundlePretty e)

p2 :: IO ()
p2 = do
  xs <- parse (pair `sepEndBy` eol) "" <$> input
  case xs of
    Right cs -> do
      let x1 = minimum $ fmap fst cs
          x2 = maximum $ fmap fst cs
          y1 = minimum $ fmap snd cs
          y2 = maximum $ fmap snd cs
          ps = do
            x <- [x1 .. x2]
            y <- [y1 .. y2]
            pure (x, y)
          totalDist p = sum $ fmap (dist1 p) cs
          ds = fmap totalDist ps
      print (length $ filter (< 10000) ds)
    Left e -> putStr (errorBundlePretty e)

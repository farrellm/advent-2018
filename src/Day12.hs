{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day12 (p1, p2) where

import AdventPrelude hiding ()
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Text as T

input :: IO Text
input = readFile "data/input-12.txt"
-- input = readFile "data/test-12.txt"

chunks :: Int -> Text -> [Text]
chunks n t
  | T.length t <= n = [t]
  | otherwise = T.take n t : chunks n (T.tail t)

step :: Map Text Text -> (Int, Text) -> (Int, Text)
step m (z, l) =
  let cs = chunks 5 ("..." <> l <> "...")
      s = T.concat $ fmap go cs
   in (z - 1, s)
  where
    go t = m M.! t
    -- go m t
    --   | M.member t m = "#"
    --   | otherwise = "."

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ x = x
iterateN n f x =
  let x' = f x
  in x' `seq` iterateN (n - 1) f x'

p1 :: IO ()
p1 = do
  (x : _ : ls) <- lines <$> input
  let [_, i] = T.splitOn ": " x
      ss = fmap (T.splitOn " => ") ls
      m = M.fromList $ fmap (\[k, v] -> (k, v)) ss
      (z, i') = iterateN 20 (step m) (0, i)
      ps = zip [z ..] (toS i' :: [Char])
  print (sum . fmap fst $ filter ((== '#') . snd) ps)

-- lookupLinear :: (Num a, Eq a) => Int -> [a] -> Int -> Maybe a
-- lookupLinear len xs i =
--   let ds = zipWith subtract xs (tail xs)
--    in go xs ds i
--   where
--     go [] _ _ = Nothing
--     go (y:ys) _ 0 = Just y
--     go (y:ys) [] j = go ys [] (j - 1)
--     go (y:ys) (d:ds) j
--       | all (== d) (take len ds) = Just (y + fromIntegral j * d)
--       | otherwise = go ys ds (j - 1)

toBits :: [Bool] -> Int
toBits = foldl' (\a b -> fromEnum b .|. shiftL a 1) 0

step' :: IntSet -> IntSet -> IntSet
step' m i =
  let js = ordNub . foldMap (\x -> fmap (+ x) [-2 .. 2]) $ IS.toList i
      ts =
        fmap (\j -> (j, toBits $ fmap ((`IS.member` i) . (+ j)) [-2 .. 2])) js
   in IS.fromList . fmap fst $ filter ((`IS.member` m) . snd) ts

p2 :: IO ()
p2 = do
  (x:_:ls) <- lines <$> input
  let [_, i] = T.splitOn ": " x
      ss = fmap ((\[a, b] -> (a, b)) . T.splitOn " => ") ls
      m =
        IS.fromList . fmap (toBits . fmap (== '#') . T.unpack . fst) $
        filter ((== "#") . snd) ss
      i' =
        IS.fromList . fmap fst . filter ((== '#') . snd) $
        zip [0 :: Int ..] (toS i)
      ns = fmap (sum . IS.toList) $ iterate (step' m) i'
  print (lookupLinear 10 ns 50000000000)

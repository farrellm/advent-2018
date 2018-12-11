{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ViewPatterns #-}

module Day11 (p1, p2) where

import AdventPrelude
import qualified Data.Sequence as Q

sn :: Int
-- sn = 18
-- sn = 42
sn = 9221

rackID :: (Int, Int) -> Int
rackID (x, _) = x + 10

power :: (Int, Int) -> Int
power c@(_, y) =
  let p = rackID c * y
      p' = p + sn
      p'' = p' * rackID c
      h = (p'' `div` 100) `mod` 10
  in h - 5

sumN :: Int -> [Int] -> [Int]
sumN _ [] = []
sumN n xs =
  let ys = take n xs
      v = sum ys
  in v : go v (Q.fromList ys) (drop n xs)
 where
   go _ _ [] = []
   go _ Empty _ = []
   go v (y :<| ys) (x : xs') =
     let v' = v - y + x
     in v' : go v' (ys |> x) xs'

maxN :: [[Int]] -> Int -> Maybe (Int, (Int, Int))
maxN ps n =
  let ss = fmap (sumN n) ps
      ss' = fmap (sumN n) $ transpose ss
      mx = maximum $ concat ss'
      (r:_) = do
        (x, qs) <- zip [1 .. (301 - n)] ss'
        (y, q) <- zip [1 .. (301 - n)] qs
        guard (q == mx)
        pure (mx, (x, y))
   in pure r

p1 :: IO ()
p1 =
  let grid = fmap (\y -> fmap (\x -> (x, y)) [1 .. 300]) [1 .. 300]
      ps = fmap (fmap power) grid
  in print (maxN ps 3)

-- maximumWith :: (Ord b, Foldable t) => (a -> b) -> t a -> a
-- maximumWith f = maximumBy (\x y -> compare (f x) (f y))

p2 :: IO ()
p2 =
  let grid = fmap (\y -> fmap (\x -> (x, y)) [1 .. 300]) [1 .. 300]
      ps = fmap (fmap power) grid
      ms = mapMaybe (maxN ps) [1 .. 300]
      ns = zipWith (\(v, (x, y)) n -> (v, (x, y, n))) ms [1 .. 300 :: Int]
  in print (maximumWith fst ns)

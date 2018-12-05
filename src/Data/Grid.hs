{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}

module Data.Grid
  ( module X
  , Grid
  , map
  , mapRows
  , mapCols
  , fold
  , sum
  , maximum
  , printGrid
  ) where

import Protolude hiding (fold, map, maximum, sum)
import qualified Protolude as P

import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed as X (assocs, elems)
import Data.Array.IO as X (freeze, thaw)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Fmt

type Grid e = A.UArray (Int, Int) e

infixl 9 !
(!) :: (A.IArray A.UArray e) => Grid e -> (Int, Int) -> e
(!) = (A.!)

map ::
     (A.IArray A.UArray e, A.IArray A.UArray e')
  => (e -> e')
  -> Grid e
  -> Grid e'
map = A.amap

fold :: (A.IArray A.UArray e) => (a -> e -> a) -> a -> Grid e -> a
fold f v = foldl' f v . elems

sum :: (A.IArray A.UArray e, Num e) => Grid e -> e
sum = fold (+) 0

maximum :: (A.IArray A.UArray e, Ord e) => Grid e -> e
maximum = P.maximum . elems

mapRows :: (A.IArray A.UArray e) => ([e] -> a) -> Grid e -> [a]
mapRows f g =
  let ((x1, y1), (x2, y2)) = A.bounds g
   in fmap (\y -> f $ fmap (\x -> g ! (x, y)) [x1 .. x2]) [y1 .. y2]

mapCols :: (A.IArray A.UArray e) => ([e] -> a) -> Grid e -> [a]
mapCols f g =
  let ((x1, y1), (x2, y2)) = A.bounds g
   in fmap (\x -> f $ fmap (\y -> g ! (x, y)) [y1 .. y2]) [x1 .. x2]

printGrid ::
     (MonadIO m, A.IArray A.UArray e, Show e, Buildable e) => Grid e -> m ()
printGrid g = do
  let ((x1, y1), (x2, y2)) = A.bounds g
      ls = A.amap (T.length . show) g
      ws = V.fromList $ mapCols P.maximum ls
  forM_ (reverse [y1 .. y2]) $ \y -> do
    let go x =
          let v = g ! (x, y)
              w = ws V.! x
           in (fmt $ padLeftF w ' ' v)
        vs = fmap go [x1 .. x2]
    putStrLn (T.intercalate " " vs)

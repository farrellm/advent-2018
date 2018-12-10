{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}

module Data.Grid.Mutable
  ( MGrid
  -- , new
  -- , new'
  , replicate
  , replicate'
  , read
  , write
  , modify
  , map
  , printGrid
  ) where

import Protolude hiding (map, modify, replicate)

import qualified Data.Array.IO as A
import qualified Data.Array.Unboxed as A
import Fmt

import qualified Data.Grid as G

type MGrid e = A.IOUArray (Int, Int) e

-- new :: (A.MArray A.IOUArray e m, MonadIO m) => Int -> Int -> m (MGrid e)
-- new w h = A.newArray_ ((0, 0), (w - 1, h - 1))

-- new' :: (A.MArray A.IOUArray e m, MonadIO m) => (Int, Int) -> (Int, Int) -> m (MGrid e)
-- new' (w1, w2) (h1, h2) = A.newArray_ ((w1, h1), (w2 - 1, h2 - 1))

replicate :: (A.MArray A.IOUArray e m) => Int -> Int -> e -> m (MGrid e)
replicate w h v = A.newArray ((0, 0), (w - 1, h - 1)) v

replicate' :: (A.MArray A.IOUArray e m) => (Int, Int) -> (Int, Int) -> e -> m (MGrid e)
replicate' (w1, w2) (h1, h2) v = A.newArray ((w1, h1), (w2 - 1, h2 - 1)) v

read :: (A.MArray A.IOUArray e m, MonadIO m) => MGrid e -> (Int, Int) -> m e
read g c = A.readArray g c

write :: (A.MArray A.IOUArray e m, MonadIO m) => MGrid e -> (Int, Int) -> e -> m ()
write g c x = A.writeArray g c x

modify ::
     (A.MArray A.IOUArray e m, MonadIO m)
  => MGrid e
  -> (e -> e)
  -> (Int, Int)
  -> m ()
modify g f c = do
  v <- read g c
  write g c (f v)

map ::
     (A.MArray A.IOUArray e m, A.MArray A.IOUArray e' m)
  => (e -> e')
  -> MGrid e
  -> m (MGrid e')
map = A.mapArray

printGrid ::
     ( A.MArray A.IOUArray e m
     , A.IArray A.UArray e
     , Show e
     , Buildable e
     , MonadIO m
     )
  => MGrid e
  -> m ()
printGrid g = G.freeze g >>= G.printGrid

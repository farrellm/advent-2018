{-# LANGUAGE FlexibleContexts #-}

module Data.Grid.Mutable
  ( MGrid
  -- , new
  -- , new'
  , replicate
  , replicate'
  , read
  , write
  , modify
  , printGrid
  ) where

import Prelude hiding (read, replicate)
import qualified Data.Array.IO as A
import Control.Monad
import Control.Monad.IO.Class

type MGrid e = A.IOUArray (Int, Int) e

-- new :: (A.MArray A.IOUArray e m, MonadIO m) => Int -> Int -> m (MGrid e)
-- new w h = A.newArray_ ((0, 0), (w - 1, h - 1))

-- new' :: (A.MArray A.IOUArray e m, MonadIO m) => (Int, Int) -> (Int, Int) -> m (MGrid e)
-- new' (w1, w2) (h1, h2) = A.newArray_ ((w1, h1), (w2 - 1, h2 - 1))

replicate ::
     (A.MArray A.IOUArray e m, A.MArray A.IOUArray e IO)
  => Int
  -> Int
  -> e
  -> m (MGrid e)
replicate w h v = A.newArray ((0, 0), (w - 1, h - 1)) v

replicate' ::
     (A.MArray A.IOUArray e m, A.MArray A.IOUArray e IO)
  => Int
  -> Int
  -> e
  -> m (MGrid e)
replicate' w h v = A.newArray ((0, 0), (w - 1, h - 1)) v

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

printGrid :: (A.MArray A.IOUArray e IO, Show e, MonadIO m) => MGrid e -> m ()
printGrid g = liftIO $ do
  ((x1, y1), (x2, y2)) <- A.getBounds g
  forM_ (reverse [y1 .. y2]) $ \y -> do
    forM_ [x1 .. x2] $ \x -> do
      v <- read g (x, y)
      putStr (show v :: String)
      putStr " "
    putStrLn ("" :: String)

module Data.Grid.Mutable
  ( MGrid(..)
  , new
  , read
  , write
  , printGrid
  ) where

import Prelude hiding (read)
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.Primitive
import Control.Monad

data MGrid s a = MGrid Int Int (MV.MVector s a)

index :: Int -> Int -> (Int, Int) -> Int
index w h (x, y) = (w + w + 1) * (y + h) + (x + w)

new :: (PrimMonad m, MV.Unbox a) => Int -> Int -> m (MGrid (PrimState m) a)
new w h = MGrid w h <$> MV.new ((w + w + 1) * (h + h + 1))

replicate ::
     (PrimMonad m, MV.Unbox a) => Int -> Int -> a -> m (MGrid (PrimState m) a)
replicate w h x = MGrid w h <$> MV.replicate ((w + w + 1) * (h + h + 1)) x

read :: (PrimMonad m, MV.Unbox a) => MGrid (PrimState m) a -> (Int, Int) -> m a
read (MGrid w h v) c = MV.read v (index w h c)

write ::
     (PrimMonad m, MV.Unbox a)
  => MGrid (PrimState m) a
  -> (Int, Int)
  -> a
  -> m ()
write (MGrid w h v) c x = MV.write v (index w h c) x

modify ::
     (PrimMonad m, MV.Unbox a)
  => MGrid (PrimState m) a
  -> (a -> a)
  -> (Int, Int)
  -> m ()
modify (MGrid w h v) f c = MV.modify v f (index w h c)

printGrid :: (Show a, MV.Unbox a) => MGrid RealWorld a -> IO ()
printGrid g@(MGrid w h _) =
  forM_ (reverse [-h .. h]) $ \y -> do
    forM_ [-w .. w] $ \x -> do
      v <- read g (x, y)
      putStr (show v :: String)
    putStrLn ("" :: String)

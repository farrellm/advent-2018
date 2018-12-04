{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day3 (p1, p2) where

import AdventPrelude hiding ()
import qualified Data.Array.IO as A
import qualified Data.Set as S

-- input' :: Text
-- input' = unlines ["#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2"]

input :: IO Text
input = readFile "data/input-3.txt"

data Rect = Rect Int Int Int Int Int
  deriving (Show)

rect :: Parser Rect
rect =
  Rect <$> (string "#" *> int) <* string " @ " <*> int <* string "," <*> int
    <* string ": " <*> int <* string "x" <*> int

type Cloth = A.IOUArray (Int, Int) Int

modifyArray :: (A.MArray a e m, A.Ix i) => a i e -> (e -> e) -> i -> m ()
modifyArray a f i = do
  v <- A.readArray a i
  A.writeArray a i (f v)

fillRect :: Cloth -> Rect -> IO ()
fillRect g (Rect _ x y w h) =
  forM_ [x .. x + w - 1] $ \x' ->
    forM_ [y .. y + h - 1] $ \y' ->
       modifyArray g (+ 1) (x', y')

countGtOne :: Cloth -> IO Int
countGtOne g = execStateT go 0
  where
    go = forM_ [0 .. 999] $ \x' -> do
      forM_ [0 .. 999] $ \y' -> do
        v <- liftIO $ A.readArray g (x', y')
        when (v > 1) $ modify (+ 1)

p1 :: IO ()
p1 = do
  xs <- parse (rect `sepEndBy` eol) "" <$> input
  -- let xs = parse (rect `sepEndBy` eol) "" input'
  case xs of
    Right ns -> do
      g <- A.newArray ((0, 0), (1000, 1000)) 0
      forM_ ns $ fillRect g
      n <- countGtOne g
      print n
    Left e -> putStr (parseErrorPretty e)

fillRectWith :: Cloth -> Rect -> IO (Set Int)
fillRectWith g (Rect v x y w h) = execStateT go S.empty
  where
    go =
      forM_ [x .. x + w - 1] $ \x' ->
        forM_ [y .. y + h - 1] $ \y' -> do
          old <- liftIO $ A.readArray g (x', y')
          unless (old == 0) $ modify (S.insert old . S.insert v)
          liftIO $ A.writeArray g (x', y') v

p2 :: IO ()
p2 = do
  xs <- parse (rect `sepEndBy` eol) "" <$> input
  -- let xs = parse (rect `sepEndBy` eol) "" input'
  case xs of
    Right ns -> do
      g <- A.newArray ((0, 0), (1000, 1000)) 0
      let vs = fmap (\(Rect v _ _ _ _) -> v) ns
      let go =
            forM_ ns $ \n -> do
              b <- liftIO $ fillRectWith g n
              modify (<> b)
      bs <- execStateT go S.empty
      print (S.fromList vs `S.difference` bs)
    Left e -> putStr (parseErrorPretty e)

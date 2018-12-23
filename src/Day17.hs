{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections, GADTs #-}

module Day17 (p1, printGrid) where

import AdventPrelude
import qualified Data.Map as M
import Lens.Micro.Platform

input :: IO Text
input = readFile "data/input-17.txt"
-- input = readFile "data/test-17.txt"

type Coord = (Int, Int)

data Barrier
  = Wall Int
         Int
         Int
  | Floor Int
          Int
          Int
  deriving (Show)

barrier :: Parser Barrier
barrier = Wall <$> ("x=" *> int) <* ", y=" <*> int <* ".." <*> int <|>
  Floor <$> ("y=" *> int) <* ", x=" <*> int <* ".." <*> int

data Cell = Clay | Stable | Flow
  deriving (Show, Eq)

type Grid = Map (Int, Int) Cell

printGrid :: Grid -> IO ()
printGrid g = do
  let ys = fmap fst $ M.keys g
      xs = fmap snd $ M.keys g
  for_ [minimum ys .. maximum ys] $ \y ->
    putStrLn $
    flip fmap [minimum xs .. maximum xs] $ \x ->
      case M.lookup (y, x) g of
        Nothing -> '.' :: Char
        Just Clay -> '#'
        Just Stable -> '~'
        Just Flow -> '|'

barrierGrid :: Barrier -> Grid
barrierGrid (Wall x y1 y2) =
  M.fromList $ fmap (\y -> ((y, x), Clay)) [y1 .. y2]
barrierGrid (Floor y x1 x2) =
  M.fromList $ fmap (\x -> ((y, x), Clay)) [x1 .. x2]

type Edge = Either Int Int

edges :: Coord -> State Grid (Edge, Edge)
edges (y, x') = (,) <$> leftEdge x' <*> rightEdge x'
  where
    leftEdge x = do
      l <- use (at (y, x - 1))
      d <- use (at (y + 1, x))
      case (l, d) of
        (_, Nothing) -> pure $ Left x
        (_, Just Flow) -> pure $ Left x
        (Just Clay, _) -> pure $ Right x
        _ -> leftEdge (x - 1)
    rightEdge x = do
      l <- use (at (y, x + 1))
      d <- use (at (y + 1, x))
      case (l, d) of
        (_, Nothing) -> pure $ Left x
        (_, Just Flow) -> pure $ Left x
        (Just Clay, _) -> pure $ Right x
        _ -> rightEdge (x + 1)

flow :: Int -> [Coord] -> State Grid ()
flow yMax ss =
  for_ ss $ \(y, x) ->
    when (y <= yMax) $ do
      d <- use (at (y + 1, x))
      case d of
        Nothing -> do
          at (y, x) .= Just Flow
          flow yMax [(y + 1, x)]
        Just Clay -> fill (y, x)
        Just Stable -> fill (y, x)
        Just Flow -> at (y, x) .= Just Flow
  where
    fill (y, x) = do
      es <- edges (y, x)
      case es of
        (Right l, Right r) -> do
          for_ [l .. r] $ \x' -> at (y, x') .= Just Stable
          fill (y - 1, x)
        (Right l, Left r) -> do
          for_ [l .. r] $ \x' -> at (y, x') .= Just Flow
          flow yMax [(y, r)]
        (Left l, Right r) -> do
          for_ [l .. r] $ \x' -> at (y, x') .= Just Flow
          flow yMax [(y, l)]
        (Left l, Left r) -> do
          for_ [l .. r] $ \x' -> at (y, x') .= Just Flow
          flow yMax [(y, l), (y, r)]

p1 :: IO ()
p1 = do
  xs <- parse (barrier `sepEndBy` eol) "" <$> input
  case xs of
    Right bs -> do
      let g = mconcat $ fmap barrierGrid bs
          yMax = maximum . fmap fst $ M.keys g
          yMin = minimum . fmap fst $ M.keys g
          g' = execState (flow yMax [(yMin, 500)]) g
      -- printGrid g'
      print (M.size $ M.filter (/= Clay) g')
      print (M.size $ M.filter (== Stable) g')
    Left e -> putStr (errorBundlePretty e)

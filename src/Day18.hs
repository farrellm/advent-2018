{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TupleSections#-}

module Day18 (p1, p2) where

import AdventPrelude
import qualified Data.Map as M
import qualified Data.Text as T

input :: IO Text
-- input = readFile "data/input-18.txt"
input = readFile "data/test-18.txt"

type Coord = (Int, Int)
type Grid = Map Coord Char

nbrs :: Coord -> State Grid [Char]
nbrs c = do
  g <- get
  pure . mapMaybe (flip M.lookup g) $ nbr8 c

stepCell :: (Coord, Char) -> State Grid (Coord, Char)
stepCell (c, x) = do
  f <- freq <$> nbrs c
  pure . (c, ) $
    case x of
      '.' ->
        if M.findWithDefault 0 '|' f >= 3
          then '|'
          else '.'
      '|' ->
        if M.findWithDefault 0 '#' f >= 3
          then '#'
          else '|'
      '#' ->
        if (M.findWithDefault 0 '#' f >= 1) .&. (M.findWithDefault 0 '|' f >= 1)
          then '#'
          else '.'
      _ -> panic "bad cell!"

step :: State Grid ()
step = put . M.fromList =<< traverse stepCell =<< (M.toList <$> get)

p1 :: IO ()
p1 = do
  ls <- lines <$> input
  let cs = fmap T.unpack ls
      g = charMap cs
      g' = execState (for_ [1 :: Int .. 10] $ \_ -> step) g
      f = freq $ M.elems g'
  printGrid g'
  print (f M.! '#' * f M.! '|')

p2 :: IO ()
p2 = do
  ls <- lines <$> input
  let cs = fmap T.unpack ls
      g = charMap cs
      gs = iterate go g
      Just g' = lookupCycle gs 1000000000
      f = freq $ M.elems g'
  print (f M.! '#' * f M.! '|')
  where
    go g = execState step g

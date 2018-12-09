{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day8 (p1, p2) where

import AdventPrelude

input :: IO Text
input = readFile "data/input-8.txt"
-- input = pure "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

data Node = Node [Node] [Int]
  deriving (Show)

next :: State [Int] Int
next = do
  xs <- get
  case xs of
    (x : xs') -> do
      put xs'
      pure x
    _ -> error "bad input"

node :: State [Int] Node
node = do
  nn <- next
  nd <- next
  ns <- traverse (\_ -> node) [1 .. nn]
  ds <- traverse (\_ -> next) [1 .. nd]
  pure (Node ns ds)

sumMD :: Node -> Int
sumMD (Node ns ms) = sum ms + (sum $ fmap sumMD ns)

p1 :: IO ()
p1 = do
  ws <- words <$> input
  let ds = fmap readInt ws
      n = evalState node ds
  print (sumMD n)

-- (!?) :: [a] -> Int -> Maybe a
-- (x : _) !? 0 = Just x
-- [] !? _ = Nothing
-- (_ : xs) !? i = xs !? (i - 1)

value :: Node -> Int
value (Node [] ms) = sum ms
value (Node ns ms) =
  sum . fmap value . mapMaybe (ns !?) $ fmap (subtract 1) ms

p2 :: IO ()
p2 = do
  ws <- words <$> input
  let ds = fmap readInt ws
      n = evalState node ds
  print (value n)

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards  #-}

module Day4 (p1, p2) where

import AdventPrelude
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

input :: IO Text
input = readFile "data/input-4.txt"
-- input = readFile "data/test-4.txt"

-- findMaxAssoc :: (Ord a) => Map k a -> Maybe (k, a)
-- findMaxAssoc = (Just . swap) <=< M.lookupMax . M.fromList . fmap swap . M.assocs

-- findMaxKey :: (Ord a) => Map k a -> Maybe k
-- findMaxKey = (Just . fst) <=< findMaxAssoc

-- findMaxValue :: (Ord a) => Map k a -> Maybe a
-- findMaxValue = (Just . snd) <=< findMaxAssoc

data Action = Begin Int | Sleep | Wake
  deriving (Show, Eq, Ord)

data Date = Date Int Int Int
  deriving (Show, Eq, Ord)

data Time = Time Int
  deriving (Show, Eq, Ord)

data Entry = Entry Date Time Action
  deriving (Show, Eq, Ord)

date :: Parser Date
date = Date <$> int <* string "-" <*> int <* string "-" <*> int

time :: Parser Time
time = Time <$> ((+ 60) <$> (string "23:" *> int) <|> string "00:" *> int)

action :: Parser Action
action =
  (string "falls asleep" *> pure Sleep) <|>
  (string "wakes up" *> pure Wake) <|>
  (Begin <$> (string "Guard #" *> int) <* string " begins shift")

entry :: Parser Entry
entry =
  Entry <$> (string "[" *> date) <* string " " <*> time <* string "] " <*> action

data GuardState = GuardState
  { _cur :: Int
  , _begin :: Int
  , _sleep :: Map Int (MV.MVector RealWorld Int)
  }

go :: GuardState -> Entry -> IO GuardState
go st (Entry _d _t (Begin k)) = pure st {_cur = k}
go st (Entry _d (Time t) Sleep) = pure st {_begin = t}
go st@GuardState {..} (Entry _d (Time t) Wake) = do
  sleep <-
    case M.lookup _cur _sleep of
      Nothing -> MV.replicate 60 0
      Just v -> pure v
  forM_ [_begin .. t - 1] $ MV.modify sleep (+ 1)
  pure st {_sleep = M.insert _cur sleep _sleep}

p1 :: IO ()
p1 = do
  xs <- parse (entry `sepEndBy` eol) "" <$> input
  case xs of
    Right ns -> do
      let ns' = sort ns
      st <- foldM go (GuardState 0 0 M.empty) ns'
      vs <- traverse V.freeze $ _sleep st
      let ss = fmap V.sum vs
          Just k = findMaxKey ss
          Just v = k `M.lookup` vs
          i = V.maxIndex v
      print (k, i, k * i)
    Left e -> putStr (errorBundlePretty e)

p2 :: IO ()
p2 = do
  xs <- parse (entry `sepEndBy` eol) "" <$> input
  case xs of
    Right ns -> do
      let ns' = sort ns
      st <- foldM go (GuardState 0 0 M.empty) ns'
      vs <- traverse V.freeze $ _sleep st
      let ms = fmap V.maximum vs
          Just (k, m) = findMaxAssoc ms
          Just j = V.findIndex (== m) =<< k `M.lookup` vs
      print (k, j, k * j)
    Left e -> putStr (errorBundlePretty e)

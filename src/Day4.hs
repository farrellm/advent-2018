{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE TupleSections, GADTs, RecordWildCards #-}

module Day4 () where

import AdventPrelude hiding ()
import qualified Data.Grid.Mutable as MG
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

input :: IO Text
input = readFile "data/input-4.txt"
-- input = readFile "data/test-4.txt"

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
go st e@(Entry d t (Begin k)) = pure st {_cur = k}
go st e@(Entry d (Time t) Sleep) = pure st {_begin = t}
go st@GuardState {..} e@(Entry d (Time t) Wake) = do
  sleep <-
    case M.lookup _cur _sleep of
      Nothing -> MV.new 60
      Just v -> pure v
  forM_ [_begin .. t - 1] $ MV.modify sleep (+ 1)
  pure st {_sleep = M.insert _cur sleep _sleep}

countMin :: GuardState -> IO (Map Int Int)
countMin st = do
  ps <- traverse go $ M.assocs (_sleep st)
  let m = M.fromList ps
  pure m
  where
    go :: (Int, MV.MVector RealWorld Int) -> IO (Int, Int)
    go (k, v) = do
      v' <- V.freeze v
      pure (k, V.sum v')

p1 :: IO ()
p1 = do
  xs <- parse (entry `sepEndBy` eol) "" <$> input
  case xs of
    Right ns -> do
      let ns' = sort ns
      st <- foldM go (GuardState 0 0 M.empty) ns'
      ss <- countMin st
      -- print ss
      let Just (m, k) = M.lookupMax . M.fromList . fmap swap $ M.assocs ss
          Just sl = k `M.lookup` _sleep st
      sl' <- V.freeze sl
      let mx = V.maximum sl'
          Just i = V.findIndex (== mx) sl'
      print (k, i, k * i)
    Left e -> putStr (parseErrorPretty e)

p2 :: IO ()
p2 = do
  xs <- parse (entry `sepEndBy` eol)"" <$> input
  case xs of
    Right ns -> do
      let ns' = sort ns
      st <- foldM go (GuardState 0 0 M.empty) ns'
      ss <- countMin st
      -- print ss
      let sls = M.assocs (_sleep st)
      sls' <- traverse (\(a, b) -> (a,) <$> V.freeze b) sls
      let mxs = fmap (second V.maximum) sls'
          mx = maximum (fmap snd mxs)
          Just i = L.findIndex ((== mx) . snd) mxs
          (k, _) = mxs L.!! i
          Just j = V.findIndex (== mx) $ snd (sls' L.!! i)
      print (k, j, k * j)
    Left e -> putStr (parseErrorPretty e)

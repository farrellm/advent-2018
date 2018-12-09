{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Day7 (p1, p2) where

import AdventPrelude
import qualified Data.Map as M
import qualified Data.Set as S
import Lens.Micro.Platform

import Text.Megaparsec.Char (alphaNumChar)

input :: IO Text
-- input = readFile "data/input-7.txt"
input = readFile "data/test-7.txt"

pair :: Parser (Char, Char)
pair =
  (,) <$> (string "Step " *> alphaNumChar) <*
  string " must be finished before step " <*>
  alphaNumChar <*
  string " can begin."

next :: (Ord a) => [(a, a)] -> Set a -> [a]
next ns vs =
  let ss = S.fromList $ fmap snd ns
      rs = S.toAscList $ S.difference vs ss
   in case rs of
        (r:_) ->
          let ns' = filter ((/= r) . fst) ns
           in r : next ns' (S.delete r vs)
        [] -> []

-- tsort :: (Ord a) => [(a, a)] -> [a]
-- tsort ps =
--   let fs = S.fromList $ fmap fst ps
--       ss = S.fromList $ fmap snd ps
--    in next ps (fs <> ss)

p1 :: IO ()
p1 = do
  xs <- parse (pair `sepEndBy` eol) "" <$> input
  case xs of
    Right ns -> print (tsort ns)
    Left e -> putStr (errorBundlePretty e)

data Task = Task Char Int
  deriving Show

data St = St
  { _time :: Int
  , _todo :: Set Char
  , _req :: [(Char, Char)]
  , _workers :: Map Int (Maybe Task)
  } deriving (Show)
makeLenses 'St

stepTask :: Task -> Task
stepTask (Task i t) = Task i (t - 1)

isDone :: State St Bool
isDone =
  (S.null <$> use todo) &&^ ((null . catMaybes . M.elems) <$> use workers)

getDone :: Task -> Maybe Char
getDone (Task i t)
  | t == 0 = Just i
  | otherwise = Nothing

clearDone :: Task -> Maybe Task
clearDone x@(Task _ t)
  | t == 0 = Nothing
  | otherwise = Just x

addTask :: Maybe Task -> State St (Maybe Task)
addTask Nothing = do
  rs <- next <$> use req <*> use todo
  case rs of
    (r : _) -> do
      todo %= S.delete r
      pure . Just $ Task r (ord r - ord 'A' + 61)
    _ -> pure Nothing
addTask j = pure j

step :: State St Int
step = do
  time += 1
  workers %= fmap (stepTask <$>)
  done <- S.fromList . mapMaybe (getDone =<<) . M.elems <$> use workers
  req %= filter (flip S.notMember done . fst)
  workers %= fmap (clearDone =<<)
  workers <~ (traverse addTask =<< use workers)
  ifM isDone (use time) step

p2 :: IO ()
p2 = do
  xs <- parse (pair `sepEndBy` eol) "" <$> input
  case xs of
    Right ns -> do
      let fs = S.fromList $ fmap fst ns
          ss = S.fromList $ fmap snd ns
          w = M.fromList $ zip [1 .. 5] $ repeat Nothing
          st = St (-1) (fs <> ss) ns w
      print (evalState step st)
    Left e -> putStr (errorBundlePretty e)

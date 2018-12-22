{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecordWildCards #-}

module Day16 (p1, p2) where

import AdventPrelude hiding ()
import qualified Data.Map as M
import qualified Data.Sequence as Q
import qualified Data.Set as S
import qualified Data.Text as T
import Lens.Micro.Platform

input :: IO Text
input = readFile "data/input-16.txt"
-- input = readFile "data/test-16.txt"

next :: State [a] (Maybe a)
next = do
  xs <- get
  case xs of
    [] -> pure Nothing
    (x : xss) -> do
      put xss
      pure (Just x)

peek :: State [a] (Maybe a)
peek = do
  xs <- get
  case xs of
    [] -> pure Nothing
    (x : _) -> do
      pure (Just x)

data Instr = Instr
  { _op :: Int
  , _a :: Int
  , _b :: Int
  , _c :: Int
  } deriving (Show)

data Test = Test
  { _in :: Seq Int
  , _out :: Seq Int
  , _instr :: Instr
  } deriving (Show)

parseReg :: Text -> Seq Int
parseReg = Q.fromList . fmap readInt . T.splitOn ", " . T.drop 1 . T.dropEnd 1

parseInstr :: Text -> Maybe Instr
parseInstr t =
  case readInt <$> T.splitOn " " t of
    [_op, _a, _b, _c] -> Just Instr {..}
    _ -> Nothing

parseTests :: State [Text] [Test]
parseTests = do
  mb <- next
  mi <- next
  ma <- next
  void next
  mn <- peek
  let mt = do
        _in <- parseReg <$> (T.stripPrefix "Before: " =<< mb)
        _instr <- parseInstr =<< mi
        _out <- parseReg <$> (T.stripPrefix "After:  " =<< ma)
        pure Test {..}
  case mt of
    Nothing -> pure []
    Just t ->
      if mn /= Just ""
        then (t :) <$> parseTests
        else do
          void next
          void next
          pure [t]

type Reg = Seq Int

eval :: Instr -> State Reg ()
eval Instr {..} = do
  reg <- get
  case _op of
    0 -> ix _c .= (reg `Q.index` _a) + (reg `Q.index` _b)
    1 -> ix _c .= (reg `Q.index` _a) + _b
    2 -> ix _c .= (reg `Q.index` _a) * (reg `Q.index` _b)
    3 -> ix _c .= (reg `Q.index` _a) * _b
    4 -> ix _c .= (reg `Q.index` _a) .&. (reg `Q.index` _b)
    5 -> ix _c .= (reg `Q.index` _a) .&. _b
    6 -> ix _c .= (reg `Q.index` _a) .|. (reg `Q.index` _b)
    7 -> ix _c .= (reg `Q.index` _a) .|. _b
    8 -> ix _c .= (reg `Q.index` _a)
    9 -> ix _c .= _a
    10 -> ix _c .= _a `gt` (reg `Q.index` _b)
    11 -> ix _c .= (reg `Q.index` _a) `gt` _b
    12 -> ix _c .= (reg `Q.index` _a) `gt` (reg `Q.index` _b)
    13 -> ix _c .= _a `eq` (reg `Q.index` _b)
    14 -> ix _c .= (reg `Q.index` _a) `eq` _b
    15 -> ix _c .= (reg `Q.index` _a) `eq` (reg `Q.index` _b)
    _ -> panic "bad op"
  where
    gt a b
      | a > b = 1
      | otherwise = 0
    eq a b
      | a == b = 1
      | otherwise = 0

evalEach :: Instr -> Reg -> [(Int, Reg)]
evalEach i r = fmap (\o -> (o, execState (eval i {_op = o}) r)) [0 .. 15]

countMatch :: Test -> Int
countMatch Test {..} = length . filter ((== _out) . snd) $ evalEach _instr _in

p1 :: IO ()
p1 = do
  ls <- lines <$> input
  let ts = evalState parseTests ls
      cs = fmap countMatch ts
  print (length $ filter (>= 3) cs)

findOp :: [Int] -> Test -> Maybe (Int, Int)
findOp ops Test {..} =
  let ms = filter ((== _out) . snd) $ evalOps _instr _in
   in case ms of
        [(c, _)] -> Just (_op _instr, c)
        _ -> Nothing
  where
    evalOps i r = fmap (\o -> (o, execState (eval i {_op = o}) r)) ops

solvePerm :: [Test] -> Map Int Int
solvePerm ts = go [0 .. 15]
  where
    go [] = M.empty
    go ops =
      let ss = M.fromList $ mapMaybe (findOp ops) ts
          os = S.fromList $ M.elems ss
          ops' = filter (`S.notMember` os) ops
       in ss <> go ops'

p2 :: IO ()
p2 = do
  ls <- lines <$> input
  let (ts, rs) = runState parseTests ls
      p = solvePerm ts
      rs' = fmap (permOp p) $ mapMaybe parseInstr rs
      reg = execState (for_ rs' eval) $ Q.replicate 4 0
  print (reg)
  where
    permOp p i = i {_op = p M.! _op i}

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE TupleSections, GADTs, RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Day15 (p1, p2) where

import AdventPrelude hiding (ap)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Sequence as Q
import qualified Data.Set as S
import qualified Data.HashSet as H
import qualified Data.Text as T
import Lens.Micro.Platform

input :: IO Text
input = readFile "data/input-15.txt"
-- input = readFile "data/test-15-5.txt"

data Species = Elf | Gob
  deriving (Show, Eq, Ord)

data Unit = Unit
  { _sp :: Species
  , _hp :: Int
  , _ap :: Int
  } deriving (Show, Eq, Ord)

makeLenses 'Unit

type Coord = (Int, Int)

dirs :: [Dir4]
dirs = [U, L, R, D]

move :: Coord -> Dir4 -> Coord
move (y, x) L = (y, x - 1)
move (y, x) R = (y, x + 1)
move (y, x) U = (y - 1, x)
move (y, x) D = (y + 1, x)

allDist :: (Coord -> Bool) -> Int -> Map Coord Int -> Set Coord -> Map Coord Int
allDist f d ds ps =
  let g = f ||^ (`M.member` ds)
      ps' =
        S.filter (not . g) $
        S.fromList . concatMap (\p -> fmap (move p) dirs) $ toList ps
      ds' = M.fromSet (const d) ps'
   in if S.null ps'
        then ds
        else allDist f (d + 1) (ds <> ds') ps'

isNbr :: Foldable t => t Coord -> Coord -> Bool
isNbr ps p = any (\q -> dist1 p q == 1) ps

stepUnit ::
     Set Coord
  -> Map Coord Unit
  -> (Coord, Unit)
  -> Map Coord Unit
  -> Either [Unit] (Map Coord Unit, (Coord, Unit), Map Coord Unit)
stepUnit ws moved (p, u) toMove = do
  let f = (`S.member` ws) ||^ (`M.member` moved) ||^ (`M.member` toMove)
      isEnemy v = _sp v /= _sp u
      other = moved <> toMove
      es = M.keys $ M.filter isEnemy other
      ds = allDist f 1 (M.singleton p 0) (S.singleton p)
      ds' = M.filterWithKey (\p _ -> isNbr es p) ds
      ts = S.fromList . fmap swap $ M.toList ds'
  when (null es) $ Left (M.elems moved ++ u : M.elems toMove)
  case S.lookupMin ts of
    Nothing -> pure (moved, (p, u), toMove)
    Just (_, q) ->
      let nbrs r = filter (not . f) $ fmap (move r) dirs
          dist a b
            | a == p =
              let (y1, x1) = a
                  (y2, x2) = b
               in case (y2 - y1, x2 - x1) of
                    (-1, _) -> 1
                    (_, -1) -> 2
                    (_, 1) -> 3
                    (1, _) -> 4
          dist a b
            | b == p = dist b a
          dist _ _ = 4
          path = aStar (H.fromList . nbrs) dist ((* 4) . dist1 q) (== q) p
          p' =
            case path of
              Just (x:_) -> x
              _ -> p
          nes =
            M.filter isEnemy .
            M.fromList .
            fmap (\(r, w) -> ((_hp w, r), w)) .
            mapMaybe (\r -> (r, ) <$> M.lookup r other) $
            fmap (move p') dirs
       in case M.lookupMin nes of
            Nothing -> pure (moved, (p', u), toMove)
            Just ((_, r), _) ->
              let hit w = w & hp -~ (u ^. ap)
                  adj m = M.filter ((> 0) . _hp) $ M.adjust hit r m
                  moved' = adj moved
                  toMove' = adj toMove
               in pure (moved', (p', u), toMove')

step :: Int -> Set Coord -> Map Coord Unit -> (Int, Int)
step i ws us =
  case tweakM (stepUnit ws) us of
    Right us' -> step (i + 1) ws us'
    Left us' ->
      let s = sum (fmap _hp us')
       in (length us', i * s)

p1 :: IO ()
p1 = do
  ls <- lines <$> input
  let cs = fmap T.unpack ls
      ps =
        concat .
        fmap (\(r, xs) -> fmap (\(c, x) -> ((r, c), x)) $ zip [0 ..] xs) $
        zip [0 ..] cs
      posSet c = S.fromList $ fmap fst $ filter ((== c) . snd) ps
      ws = posSet '#'
      es = M.fromSet (const Unit {_sp = Elf, _hp = 200, _ap = 3}) $ posSet 'E'
      gs = M.fromSet (const Unit {_sp = Gob, _hp = 200, _ap = 3}) $ posSet 'G'
  print (step 0 ws (es <> gs))

p2 :: IO ()
p2 = do
  ls <- lines <$> input
  let cs = fmap T.unpack ls
      ps =
        concat .
        fmap (\(r, xs) -> fmap (\(c, x) -> ((r, c), x)) $ zip [0 ..] xs) $
        zip [0 ..] cs
      posSet c = S.fromList $ fmap fst $ filter ((== c) . snd) ps
      ws = posSet '#'
      es = M.fromSet (const Unit {_sp = Elf, _hp = 200, _ap = 34}) $ posSet 'E'
      gs = M.fromSet (const Unit {_sp = Gob, _hp = 200, _ap = 3}) $ posSet 'G'
  print (M.size es)
  print (step 0 ws (es <> gs))

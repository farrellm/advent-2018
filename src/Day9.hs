{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Day9 (p1, p2) where

import AdventPrelude
import qualified Data.Map as M
import Lens.Micro.Platform

input :: (Int, Int)
-- input = "455 players; last marble is worth 71223 points"
input = (455, 71223)
-- input = (9, 25)

data Marbles = Marbles [Int] Int [Int]
  deriving (Show)

frwd :: Marbles -> Marbles
frwd ms@(Marbles [] _ []) = ms
frwd (Marbles pr m (n : sf)) = Marbles (m : pr) n sf
frwd (Marbles pr m []) = frwd (Marbles [] m (reverse pr))

back :: Marbles -> Marbles
back ms@(Marbles [] _ []) = ms
back (Marbles (n : pr) m sf) = Marbles pr n (m : sf)
back (Marbles [] m sf) = back (Marbles (reverse sf) m [])

ins :: Int -> Marbles -> Marbles
ins n (Marbles pr m sf) = Marbles (m : pr) n sf

rmv :: Marbles -> (Int, Marbles)
rmv (Marbles pr m (n : sf)) = (m, Marbles pr n sf)
rmv (Marbles pr m []) = rmv (Marbles [] m (reverse pr))

data St = St { _mrbls :: Marbles
             , _player :: Int
             , _next :: Int
             , _nPlayers :: Int
             , _lst :: Int
             , _score :: Map Int Int}
  deriving (Show)

makeLenses 'St

go :: State St (Maybe Int)
go = do
  n <- use next
  l <- use lst
  nPl <- use nPlayers
  player %= (`mod` nPl) . (+ 1)
  next %= (+ 1)
  case (n > (l + 1), n `mod` 23) of
    (True, _) -> findMaxValue <$> use score
    (_, 0) -> do
      p <- use player
      score %= M.insertWith (+) p n
      for_ [1 :: Int .. 7] $ \_ -> mrbls %= back
      (m, ms') <- rmv <$> use mrbls
      score %= M.insertWith (+) p m
      mrbls .= ms'
      go
    _ -> do
      mrbls %= frwd
      mrbls <~ ins n <$> use mrbls
      go

p1 :: IO ()
p1 = do
  let (nPl, l) = input
      s = St (Marbles [] 0 []) 0 1 nPl l M.empty
  print (evalState go s)

p2 :: IO ()
p2 = do
  let (nPl, l) = input
      s = St (Marbles [] 0 []) 0 1 nPl (l * 100) M.empty
  print (evalState go s)

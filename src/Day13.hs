{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE TupleSections, RecordWildCards, TemplateHaskell #-}

module Day13 (p1, p2) where

import AdventPrelude
import qualified Data.Map as M
import Lens.Micro.Platform

input :: IO Text
input = readFile "data/input-13.txt"
-- input = readFile "data/test-13.txt"
-- input = readFile "data/test-13-2.txt"

data Turn = TL | TS | TR
  deriving (Show)

nextTurn :: Turn -> Turn
nextTurn TL = TS
nextTurn TS = TR
nextTurn TR = TL

data Cart = Cart
  { _x :: Int
  , _y  :: Int
  , _turn :: Turn
  , _dir :: Dir4
  } deriving (Show)
makeLenses 'Cart

move :: MonadState Cart m => m ()
move = do
  d <- use dir
  case d of
    L -> x %= subtract 1
    R -> x %= (+ 1)
    U -> y %= subtract 1
    D -> y %= (+ 1)

corner :: Char -> Dir4 -> Dir4
corner '/' L = D
corner '/' U = R
corner '/' R = U
corner '/' D = L
corner '\\' L = U
corner '\\' U = L
corner '\\' R = D
corner '\\' D = R
corner _ d = d

turnDir :: Turn -> Dir4 -> Dir4
turnDir TL L = D
turnDir TR L = U
turnDir TS L = L
turnDir TL R = U
turnDir TR R = D
turnDir TS R = R
turnDir TL U = L
turnDir TR U = R
turnDir TS U = U
turnDir TL D = R
turnDir TR D = L
turnDir TS D = D

stepCart :: Map (Int, Int) Char -> State Cart ()
stepCart m = do
  move
  p <- (,) <$> use x <*> use y
  case M.lookup p m of
    Just '+' -> do
      dir <~ turnDir <$> use turn <*> use dir
      turn %= nextTurn
    Just c -> dir %= corner c
    Nothing -> pass

mkCart :: ((Int, Int), Char) -> Maybe Cart
mkCart ((x', y'), d) = do
  d' <- case d of
        '>' -> Just R
        'v' -> Just D
        '<' -> Just L
        '^' -> Just U
        _ -> Nothing
  Just Cart {_x = x', _y = y', _dir = d', _turn = TL}

getPos :: Cart -> (Int, Int)
getPos Cart{..} = (_x, _y)

checkCrash :: MonadError (Int, Int) m => [Cart] -> m ()
checkCrash cs = do
  case findDup $ fmap getPos cs of
    Nothing -> pure ()
    Just p -> throwError p

step ::
     (MonadError (Int, Int) m, MonadState [Cart] m)
  => Map (Int, Int) Char
  -> m ()
step m = do
  cs <- get
  cs' <- go [] $ sortOn (swap . getPos) cs
  put cs'
  where
    go rs [] = pure rs
    go rs (c:cs) = do
      let c' = execState (stepCart m) c
      checkCrash (rs ++ c' : cs)
      go (c' : rs) cs

p1 :: IO ()
p1 = do
  ls <- lines <$> input
  let xs =
        concatMap
          (\(y', l) -> fmap (\(x', c) -> ((x', y'), c)) (zip [0 ..] (toS l)))
          (zip [0 ..] ls)
      m = M.fromList $ filter ((/= ' ') . snd) xs
      cs = mapMaybe mkCart xs
      r = runExcept $ evalStateT (for_ [1 :: Int ..] $ \_ -> step m) cs
  print r

-- findDelete :: (a -> Bool) -> [a] -> Maybe [a]
-- findDelete _ [] = Nothing
-- findDelete p (x : xs)
--   | p x = Just xs
--   | otherwise = (x :) <$> findDelete p xs

clearCrash :: Cart -> [Cart] -> Maybe [Cart]
clearCrash c cs =
  let p = getPos c
  in findDelete ((== p) . getPos) cs

step' ::
     (MonadError (Int, Int) m, MonadState [Cart] m)
  => Map (Int, Int) Char
  -> m ()
step' m = do
  cs <- get
  cs' <- go [] $ sortOn (swap . getPos) cs
  case cs' of
    [] -> throwError $ (-1, -1)
    [c] -> throwError $ getPos c
    _ -> put cs'
  where
    go rs [] = pure rs
    go rs (c:cs) = do
      let c' = execState (stepCart m) c
      case (clearCrash c' rs, clearCrash c' cs) of
        (Nothing, Nothing) -> go (c' : rs) cs
        (Just rs', Nothing) -> go rs' cs
        (Nothing, Just cs') -> go rs cs'
        (Just rs', Just cs') -> go rs' cs'

p2 :: IO ()
p2 = do
  ls <- lines <$> input
  let xs =
        concatMap
          (\(y', l) -> fmap (\(x', c) -> ((x', y'), c)) (zip [0 ..] (toS l)))
          (zip [0 ..] ls)
      m = M.fromList $ filter ((/= ' ') . snd) xs
      cs = mapMaybe mkCart xs
      r = runExcept $ evalStateT (for_ [1 :: Int ..] $ \_ -> step' m) cs
  print r

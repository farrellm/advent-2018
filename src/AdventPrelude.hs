{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns, TypeFamilies  #-}

module AdventPrelude
  ( module Protolude
  , module Prelude

  -- re-exports
  , module X

  -- locally defined
  , Dir4(..)
  , Dir8(..)
  , putTxt
  , putTxtLn
  , enum
  , findDup
  , findDups
  , cycleLen
  , freq
  , findMaxAssoc
  , findMaxKey
  , findMaxValue
  , findMinAssoc
  , findMinKey
  , findMinValue
  , findDelete
  , maximumWith
  , minimumWith
  , juxt
  , juxt3
  , digits
  , md5
  , sha1
  , nbr4
  , nbr8
  , mv4
  , mv8
  , dist1
  , bounds
  , words
  , lines
  , unwords
  , unlines
  , unpack
  , readInt
  , sortChar
  , tsort
  , (!?)
  , Tweakable
  , tweak
  , tweakM
  , lookupCycle
  , lookupLinear

  -- parser
  , Parser
  , int
  -- , double
  ) where

import Protolude hiding (head, try)
import Prelude (String, error, head, tail, last)

import Control.Monad.Primitive as X (PrimMonad, PrimState, RealWorld)
import Data.Graph.AStar as X
import Data.Ix as X (Ix, range, inRange, rangeSize)

import Crypto.Hash
-- import qualified Data.List as L
import Data.List as X ((!!))
import qualified Data.Map as M
import Data.Sequence as X (Seq(..), (<|), (|>))
import qualified Data.Set as S
import qualified Data.String as Str
import qualified Data.Text as T

import qualified Text.Megaparsec as Mp
import Text.Megaparsec as X
  ( eof
  , lookAhead
  , notFollowedBy
  , parse
  , errorBundlePretty
  , try
  )
import Text.Megaparsec.Char as X
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators as X
  ( between
  , endBy
  , endBy1
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  )

putTxt :: Text -> IO ()
putTxt = putStr

putTxtLn :: Text -> IO ()
putTxtLn = putStrLn

enum :: (Enum a, Bounded a) => [a]
enum = enumFromTo minBound maxBound

findDup :: Ord b => [b] -> Maybe b
findDup xs =
  let ss = scanl' (flip S.insert) S.empty xs
  in fst <$> find (uncurry S.member) (zip xs ss)

findDups :: Ord b => [b] -> [b]
findDups xs =
  let ss = scanl' (flip S.insert) S.empty xs
  in fst <$> filter (uncurry S.member) (zip xs ss)

cycleLen :: Ord a => [a] -> Maybe Int
cycleLen xs =
  let ms = scanl' f M.empty $ zip xs [0 ..]
      zs = zip [0 ..] $ zip xs ms
   in do (i, (v, m)) <- find (uncurry M.member . snd) zs
         j <- M.lookup v m
         pure (i - j)
  where
    f m (k, v) = M.insert k v m

freq :: (Traversable t, Ord a) => t a -> Map a Int
freq cs = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty cs

findMaxAssoc :: (Ord a) => Map k a -> Maybe (k, a)
findMaxAssoc = (Just . swap) <=< M.lookupMax . M.fromList . fmap swap . M.assocs

findMaxKey :: (Ord a) => Map k a -> Maybe k
findMaxKey = (Just . fst) <=< findMaxAssoc

findMaxValue :: (Ord a) => Map k a -> Maybe a
findMaxValue = (Just . snd) <=< findMaxAssoc

findMinAssoc :: (Ord a) => Map k a -> Maybe (k, a)
findMinAssoc = (Just . swap) <=< M.lookupMin . M.fromList . fmap swap . M.assocs

findMinKey :: (Ord a) => Map k a -> Maybe k
findMinKey = (Just . fst) <=< findMinAssoc

findMinValue :: (Ord a) => Map k a -> Maybe a
findMinValue = (Just . snd) <=< findMinAssoc

findDelete :: (a -> Bool) -> [a] -> Maybe [a]
findDelete _ [] = Nothing
findDelete p (x : xs)
  | p x = Just xs
  | otherwise = (x :) <$> findDelete p xs

maximumWith :: (Ord b, Foldable t) => (a -> b) -> t a -> a
maximumWith f = maximumBy (\x y -> compare (f x) (f y))

minimumWith :: (Ord b, Foldable t) => (a -> b) -> t a -> a
minimumWith f = minimumBy (\x y -> compare (f x) (f y))

juxt :: (a -> b) -> (a -> c) -> a -> (b, c)
juxt f g x = (f x, g x)

juxt3 :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b, c, d)
juxt3 f g h x = (f x, g x, h x)

digits :: Int -> [Int]
digits i = fmap (subtract (ord '0') . ord) $ show i

stringHash ::
     (HashAlgorithm alg, StringConv s ByteString, StringConv String s)
  => alg
  -> s
  -> s
stringHash hasher msg = show $ hashWith hasher (toS msg :: ByteString)

md5 :: (StringConv s ByteString, StringConv String s) => s -> s
md5 = stringHash MD5

sha1 :: (StringConv s ByteString, StringConv String s) => s -> s
sha1 = stringHash SHA1

data Dir4 = L | R | U | D
  deriving (Show, Eq, Ord, Enum, Bounded)

data Dir8 = LL | LU | LD | RR | RU | RD | UU | DD
  deriving (Show, Eq, Ord, Enum, Bounded)

nbr4 :: (Num a) => (a, a) -> [(a, a)]
nbr4 (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

nbr8 :: (Num a) => (a, a) -> [(a, a)]
nbr8 (x, y) =
  [ (x + 1, y)
  , (x + 1, y + 1)
  , (x, y + 1)
  , (x - 1, y + 1)
  , (x - 1, y)
  , (x - 1, y - 1)
  , (x, y - 1)
  , (x + 1, y - 1)
  ]

mv4 :: (Enum a, Enum b) => (a, b) -> Dir4 -> (a, b)
mv4 (x, y) L = (pred x, y)
mv4 (x, y) R = (succ x, y)
mv4 (x, y) U = (x, succ y)
mv4 (x, y) D = (x, pred y)

mv8 :: (Enum a, Enum b) => (a, b) -> Dir8 -> (a, b)
mv8 (x, y) LL = (pred x, y)
mv8 (x, y) LU = (pred x, succ y)
mv8 (x, y) LD = (pred x, pred y)
mv8 (x, y) RR = (succ x, y)
mv8 (x, y) RU = (succ x, succ y)
mv8 (x, y) RD = (succ x, pred y)
mv8 (x, y) UU = (x, succ y)
mv8 (x, y) DD = (x, pred y)

dist1 :: Num n => (n, n) -> (n, n) -> n
dist1 (a, b) (c, d) = abs (a - c) + abs (b - d)

bounds :: (Ord n) => [(n, n)] -> ((n, n), (n, n))
bounds cs =
  let x1 = minimum $ fmap fst cs
      x2 = maximum $ fmap fst cs
      y1 = minimum $ fmap snd cs
      y2 = maximum $ fmap snd cs
   in ((x1, x2), (y1, y2))

type Parser = Mp.Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

int :: Parser Int
int = L.signed sc L.decimal

-- double :: Parser Double
-- double = L.signed sc L.float

class Chars a where
  words :: a -> [a]
  lines :: a -> [a]
  unwords :: [a] -> a
  unlines :: [a] -> a
  unpack :: a -> [Char]

instance Chars String where
  words = Str.words
  lines = Str.lines
  unwords = Str.unwords
  unlines = Str.unlines
  unpack = identity

instance Chars Text where
  words = T.words
  lines = T.lines
  unwords = T.unwords
  unlines = T.unlines
  unpack = T.unpack

sortChar :: (StringConv s String, StringConv String s) => s -> s
sortChar w = toS $ sort (toS w :: String)

tsort :: (Ord a) => [(a, a)] -> [a]
tsort ps =
  let fs = S.fromList $ fmap fst ps
      ss = S.fromList $ fmap snd ps
   in next ps (fs <> ss)
  where
    next ns vs =
      let ss = S.fromList $ fmap snd ns
          rs = S.toAscList $ S.difference vs ss
       in case rs of
            (r:_) ->
              let ns' = filter ((/= r) . fst) ns
               in r : next ns' (S.delete r vs)
            [] -> []

readInt :: (StringConv s Text) => s -> Int
readInt s =
  case parse int "" (toS s) of
    Left e -> error (errorBundlePretty e)
    Right i -> i

infixl 9  !?

{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
(x : _) !? 0 = Just x
[] !? _ = Nothing
(_ : xs) !? i = xs !? (i - 1)

class Tweakable t where
  type EntryIn t b
  type EntryOut t b
  tweak :: (t b -> EntryIn t b -> t b -> (t b, EntryOut t b, t b)) -> t b -> t b
  tweakM ::
       (Monad m)
    => (t b -> EntryIn t b -> t b -> m (t b, EntryOut t b, t b))
    -> t b
    -> m (t b)

instance (Ord a) => Tweakable (Map a) where
  type EntryIn (Map a) b = (a, b)
  type EntryOut (Map a) b = (a, b)
  tweak f m =
    case M.minViewWithKey m of
      Nothing -> M.empty
      Just (e, m') -> go M.empty e m'
    where
      go done x todo =
        let (done', x', todo') = f done x todo
            done'' = uncurry M.insert x' done'
         in case M.minViewWithKey todo' of
              Nothing -> done''
              Just (x'', todo'') -> go done'' x'' todo''
  tweakM f m =
    case M.minViewWithKey m of
      Nothing -> pure M.empty
      Just (e, m') -> go M.empty e m'
    where
      go done x todo = do
        (done', x', todo') <- f done x todo
        let done'' = uncurry M.insert x' done'
        case M.minViewWithKey todo' of
          Nothing -> pure done''
          Just (x'', todo'') -> go done'' x'' todo''

instance Tweakable Seq where
  type EntryIn Seq b = (Int, b)
  type EntryOut Seq b = b
  tweak _ Empty = Empty
  tweak f (e :<| m) = go 0 Empty e m
    where
      go i done x todo =
        let (done', x', todo') = f done (i, x) todo
            done'' = done' :|> x'
         in case todo' of
              Empty -> done''
              (x'' :<| todo'') -> go (i + 1) done'' x'' todo''
  tweakM _ Empty = pure Empty
  tweakM f (e :<| m) = go 0 Empty e m
    where
      go i done x todo = do
        (done', x', todo') <- f done (i, x) todo
        let done'' = done' :|> x'
        case todo' of
          Empty -> pure done''
          (x'' :<| todo'') -> go (i + 1) done'' x'' todo''

lookupCycle :: Ord a => [a] -> Int -> Maybe a
lookupCycle xs i =
  case go xs 0 M.empty of
    Right v -> v
    Left (i0, l) -> Just (xs !! (((i - i0) `mod` l) + i0))
  where
    go :: Ord a => [a] -> Int -> Map a Int -> Either (Int, Int) (Maybe a)
    go [] _ _ = Right Nothing
    go (y:ys) j m
      | i == j = Right $ Just y
      | otherwise =
        case M.lookup y m of
          Nothing -> go ys (j + 1) (M.insert y j m)
          Just k -> Left (k, j - k)

lookupLinear :: (Num a, Eq a) => Int -> [a] -> Int -> Maybe a
lookupLinear len xs i =
  let ds = zipWith subtract xs (tail xs)
   in go xs ds i
  where
    go [] _ _ = Nothing
    go (y:_) _ 0 = Just y
    go (_:ys) [] j = go ys [] (j - 1)
    go (y:ys) (d:ds) j
      | all (== d) (take len ds) = Just (y + fromIntegral j * d)
      | otherwise = go ys ds (j - 1)

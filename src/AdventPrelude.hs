{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

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
  , md5
  , sha1
  , nbr4
  , nbr8
  , words
  , lines
  , readInt

  -- parser
  , Parser
  , int
  -- , double
  ) where

import Protolude hiding (head, tail, try)
import Prelude (String, error, head, tail, last)

import Control.Monad.Primitive as X (PrimMonad, PrimState, RealWorld)
import Data.Graph.AStar as X

import Crypto.Hash
import qualified Data.Text as T
import qualified Data.String as S

import qualified Text.Megaparsec as Mp
import Text.Megaparsec as X
  ( eof
  , lookAhead
  , notFollowedBy
  , parse
  , parseErrorPretty
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

stringHash ::
     (HashAlgorithm alg, StringConv s ByteString, StringConv ByteString s)
  => alg
  -> s
  -> s
stringHash hasher msg =
  let msg' = toS msg :: ByteString
      hash = show (hashWith hasher msg') :: ByteString
   in toS hash

md5 :: (StringConv s ByteString, StringConv ByteString s) => s -> s
md5 = stringHash MD5

sha1 :: (StringConv s ByteString, StringConv ByteString s) => s -> s
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

type Parser = Mp.Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

int :: Parser Int
int = L.signed sc L.decimal

double :: Parser Double
double = L.signed sc L.float

class Chars a where
  words :: a -> [a]
  lines :: a -> [a]

instance Chars String where
  words = S.words
  lines = S.lines

instance Chars Text where
  words = T.words
  lines = T.lines

sortChar :: (StringConv s String, StringConv String s) => s -> s
sortChar w = toS $ sort (toS w :: String)

readInt :: (StringConv s Text) => s -> Int
readInt s =
  case parse int "" (toS s) of
    Left e -> error (parseErrorPretty e)
    Right i -> i

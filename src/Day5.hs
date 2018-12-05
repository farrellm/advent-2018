{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day5 (p1, p2) where

import AdventPrelude
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Set as S

input :: IO Text
input = readFile "data/input-5.txt"

cancl :: [Char] -> [Char]
cancl [] = []
cancl [c] = [c]
cancl (a:b:rs) =
  if a /= b && C.toLower a == C.toLower b
    then cancl rs
    else a : cancl (b : rs)

findFix :: (Eq a) => (a -> a) -> a -> a
findFix f v =
  let v' = f v
   in if v == v'
        then v
        else findFix f v'

p1 :: IO ()
p1 = do
  l <- unpack <$> input
  let l' = findFix cancl l
  print (length l')

-- findMinAssoc :: (Ord a) => Map k a -> Maybe (k, a)
-- findMinAssoc = (Just . swap) <=< M.lookupMin . M.fromList . fmap swap . M.assocs

p2 :: IO ()
p2 = do
  l <- unpack <$> input
  -- let l = "dabAcCaCBAcCcaDA" :: String
  let cs = S.fromList $ fmap C.toLower l
      ls = M.fromSet (\c -> filter ((/= c) . C.toLower) l) cs
      ls' = fmap (findFix cancl) ls
      Just (k, _) = findMinAssoc (fmap length ls')
  print (k, ls' M.! k, length (ls' M.! k))

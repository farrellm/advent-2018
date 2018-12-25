{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Day20 (p1) where

import AdventPrelude hiding ()
import qualified Data.Map as M
import qualified Data.Set as S

input :: IO Text
input = readFile "data/input-20.txt"
-- input = pure "^ENWWW(NEEE|SSE(EE|N|))$"
-- input = pure "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
-- input = pure "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

data Hall
  = Single [Char]
  | Series [Hall]
  | Fork [Hall]
  deriving (Show)

halls :: Parser Hall
halls = Series <$> some hall

fork :: Parser Hall
fork =
  Fork <$>
  (try (halls `sepBy` "|") <|> (<> [Single ""]) <$> (halls `sepEndBy` "|"))

hall :: Parser Hall
hall = "(" *> fork <* ")" <|> Single <$> some letterChar

type Coord = (Int, Int)

mv :: Char -> Coord -> Coord
mv 'N' (r, c) = (r - 1, c)
mv 'S' (r, c) = (r + 1, c)
mv 'E' (r, c) = (r, c + 1)
mv 'W' (r, c) = (r, c - 1)
mv _ _ = panic "bad direction"

explore :: Set Coord -> Hall -> State (Map Coord Char) (Set Coord)
explore cs (Single []) = pure cs
explore cs (Single (d:ds)) = do
  let cs' = S.map (mv d) cs
      cs'' = S.map (mv d . mv d) cs
  modify (M.fromSet (const '+') cs' <>)
  modify (M.fromSet (const '#') cs'' <>)
  explore cs'' (Single ds)
explore cs (Series []) = pure cs
explore cs (Series (h:hs)) = do
  cs' <- explore cs h
  explore cs' (Series hs)
explore cs (Fork hs) = do
  S.unions <$> traverse (explore cs) hs

nbrs :: Coord -> [(Coord, Coord)]
nbrs c = fmap (\d -> (mv4 c d, mv4 (mv4 c d) d)) enum

dists :: Map Coord Char -> Int -> Set Coord -> Map Coord Int -> Map Coord Int
dists m d cs ds =
  let ds' = ds <> M.fromSet (const d) cs
      ps = nbrs =<< toList cs
      ps' = filter ((flip M.member m) . fst) ps
      cs' = S.fromList . filter (flip M.notMember ds') $ fmap snd ps'
   in if S.null cs'
        then ds'
        else dists m (d + 1) cs' ds'

p1 :: IO ()
p1 = do
  xs <- parse ("^" *> halls <* "$") "" <$> input
  case xs of
    Right hs -> do
      let o = S.singleton (0, 0)
          m = execState (explore o hs) (M.singleton (0, 0) 'X')
          d = dists m 0 o M.empty
      -- print hs
      -- printGrid m
      print (findMaxAssoc d)
      print (M.size $ M.filter (>= 1000) d)
    Left e -> putStr (errorBundlePretty e)

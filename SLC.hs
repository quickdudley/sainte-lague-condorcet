module SLC (
  House,
  Tally,
  Ballot,
  quotient,
  addtoHouse,
  fillHouse
 ) where

import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Ratio
import qualified Data.Map as M

quotient s = 1 / (2*(toRational s) + 1)

type House = M.Map String Int

type Tally = M.Map (String,String) Rational

type Ballot = [(Int,String)]

tally :: House -> [Ballot] -> Tally
tally house ballots = let
  ts = map tally1 ballots
  tally1 b' = let
    b = (handicap 0 . groupBy ((==) `on` fst) . sort) b'
    t = catMaybes $ pairsWith pf b
    in M.fromListWith (+) t
  handicap :: Int -> [Ballot] -> [(Int,String,Rational)]
  handicap _ [] = []
  handicap n (l:r) = let
    n' = n + (sum $ mapMaybe (flip M.lookup house . snd) l)
    h (r,p) = (r, p, quotient n')
    in map h l ++ handicap n' r
  pf :: (Int,String,Rational) -> (Int,String,Rational) -> Maybe ((String,String),Rational)
  pf (ra,pa,qa) (rb,pb,qb) = case ra `compare` rb of
    LT -> Just ((pa,pb),qa)
    GT -> Just ((pb,pa),qb)
    EQ -> Nothing
  in foldr1 (M.unionWith (+)) ts

rpWinner :: Tally -> String
rpWinner t = let
  tc = M.mapWithKey (\(a,b) c -> c - (maybe 0 id $ M.lookup (b,a) t)) t
  tl = takeWhile ((>0) . snd) $ sortBy (flip compare `on` snd) $ M.toList tc
  lck g [] = g
  lck g (((a,b),s):r)
    | circle g b a = lck g r
    | otherwise = lck (M.insertWith (++) b [a] g) r
  circle g b a
    | a == b = True
    | otherwise = or $ map (circle g b) $ maybe [] id $ M.lookup a g
  lg = lck M.empty tl
  ((s,_),_) = head tl
  sk q = case M.lookup q lg of
    Nothing -> q
    Just q' -> sk $ head q'
  in sk s

addtoHouse :: [Ballot] -> House -> House
addtoHouse b h = M.insertWith (+) (rpWinner $ tally h b) 1 h

fillHouse b = iterate (addtoHouse b) M.empty

pairsWith _ [] = []
pairsWith f (a:r) = zipWith f (repeat a) r ++ pairsWith f r

pairs = pairsWith (,)

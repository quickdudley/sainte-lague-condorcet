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
import Data.Number.Symbolic
import Data.Ratio
import qualified Data.Map as M

quotient s = 1 / (2*s + 1)

type House = M.Map String Int

type Tally = M.Map (String,String) (Sym Rational)

type Ballot = [(Int,String)]

tally :: [Ballot] -> Tally
tally ballots = let
  ts = map tally1 ballots
  tally1 b' = let
    b = (handicap 0 . groupBy ((==) `on` fst) . sort) b'
    t = catMaybes $ pairsWith pf b
    in M.fromListWith (+) t
  handicap :: Sym Rational -> [Ballot] -> [(Int,String,Sym Rational)]
  handicap _ [] = []
  handicap n (l:r) = let
    n' = n + (sum $ map (var . snd) l)
    q = quotient n'
    h (r,p) = (r, p, q)
    in map h l ++ handicap n' r
  pf :: (Int,String,Sym Rational) -> (Int,String,Sym Rational) -> Maybe ((String,String),Sym Rational)
  pf (ra,pa,qa) (rb,pb,qb) = case ra `compare` rb of
    LT -> Just ((pa,pb),qa)
    GT -> Just ((pb,pa),qb)
    EQ -> Nothing
  in foldr1 (M.unionWith (+)) ts

rpWinner :: Tally -> House -> String
rpWinner t house = let
  tc = M.mapWithKey (\(a,b) c -> c - (maybe 0 id $ M.lookup (b,a) t)) t
  ts = M.map (unSym . substAll (M.map toRational house)) tc
  tl = takeWhile ((>0) . snd) $ sortBy (flip compare `on` snd) $ M.toList ts
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

addtoHouse :: Tally -> House -> House
addtoHouse t h = M.insertWith (+) (rpWinner t h) 1 h

fillHouse b = iterate (addtoHouse t) $ M.fromList $
  zip (map snd $ head b) (repeat 0)
 where
  t = tally b

pairsWith _ [] = []
pairsWith f (a:r) = zipWith f (repeat a) r ++ pairsWith f r

pairs = pairsWith (,)

substAll :: (Num a, Eq a) => M.Map String a -> Sym a -> Sym a
substAll v s = foldr (\(n,v') s' -> subst n (con v') s') s $ M.toList v


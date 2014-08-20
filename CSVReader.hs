module CSVReader (parseCSV, fillImplicit) where

import SLC

import Data.List
import Data.Maybe

commaSplit :: String -> [String]
commaSplit [] = []
commaSplit s = let (t,r) = cs s in trim t : commaSplit r where
  cs (',':r) = ("",r)
  cs (l:r) = let (l',r') = cs r in (l:l',r')
  trim = reverse . dropWhile ((==) ' ') . reverse . dropWhile ((==) ' ')

parseCSV :: String -> [Ballot]
parseCSV f = let
  (h:b) = lines f
  parties = commaSplit h
  ranks = map (fillImplicit . map read' . commaSplit) b
  read' "" = Nothing
  read' l = Just (read l)
  in map (flip zip parties) ranks

fillImplicit :: [Maybe Int] -> [Int]
fillImplicit l = let
  x = 1 + maximum (catMaybes l)
  in map (maybe x id) l


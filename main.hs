import SLC
import CSVReader

import Data.List
import qualified Data.Map as M
import Data.Function (on)

import System.Environment
import System.IO

main = do
  (fn:cs:_) <- getArgs
  let c = read cs
  h <- openFile fn ReadMode
  csv <- hGetContents h
  let
    b = parseCSV csv
    g = fillHouse b !! c
  putStrLn $ formatHouse g

formatHouse = intercalate "\n" .
  map f1 .
  takeWhile ((>0) . snd) .
  sortBy (flip compare `on` snd) .
  M.toList
 where
  f1 (n,c) = n ++ ": " ++ show c


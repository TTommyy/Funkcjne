module SetOperations(nD) where
-- https://en.wikipedia.org/wiki/Sardinas%E2%80%93Patterson_algorithm

import Data.Set (Set, fromList, map, unions, filter, elems, insert)
import qualified Data.Set as Set

allPostfixes :: String -> Set String
allPostfixes str = fromList [suffix i | i <- [0..length str - 1]]
  where
    suffix :: Int -> String
    suffix start = drop start str

allSetPostfixes :: Set String -> Set String
allSetPostfixes set = unions (Set.map allPostfixes set)

nD :: Set String -> Set String -> Set String
nD nSet dSet = do
  let allDpostfixes = allSetPostfixes dSet
  let f en = Set.filter (\pfD -> (en++pfD) `elem` dSet) (insert "" nSet)
  unions (Set.map f nSet)

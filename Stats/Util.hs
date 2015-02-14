module Stats.Util where

import Control.Arrow
import Data.List

-- | Like 'Data.List.transpose', but handles the case where the list
-- of lists isn't square by filling those holes with a given default
-- value.
--
-- > transposeWithFill 0 [[7],[1,2,3],[4,5],[6]] == [[7,1,4,6],[0,2,5,0],[0,3,0,0]]
transposeWithFill :: a -> [[a]] -> [[a]]
transposeWithFill a as = transpose padded
  where w = maximum (map length as)
        padded = map (\r -> r ++ replicate (w - length r) a) as

-- | Splits a list up on a given predicate.
-- The element is dropped from the result.
--
-- > splitOn (==',') "1,2,3" == ["1","2","3"]
-- > splitOn (==',') ",1,2,3," == ["","1","2","3",""]
-- > splitOn (==',') ",," == ["","",""]
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn p as = seg : sfx
  where (seg,rest) = span (not . p) as
        sfx = case rest of
                [] -> []
                (_:[]) -> [[]]
                (_:rest2) -> splitOn p rest2

-- | Factors common keys.
--
-- *> relFactor [(1,'a'),(2,'a'),(1,'b'),(2,'c'),(1,'c')]
-- [(1,"abc"),(2,"ac")]
relFactor :: Eq a => [(a,b)] -> [(a,[b])]
relFactor abs = map (\a -> (a, eq a)) as
  where as = nub (map fst abs)
        eq a = map snd $ filter ((==a) . fst) abs

-- Permutes the coordinates of a relation element.
--
-- Assumes consistent key lengths.
--
-- > *> relPermute [3] [(["a","i","x"],2),(["a","j","y"],4)]
-- > [(["x"],2),(["y"],4)]
-- > *> relPermute [1,2] [(["a","i","x"],2),(["a","j","y"],4)]
-- > [(["a","i"],2),(["a","j"],4)]
-- > *> relPermute [1] [(["a","i","x"],2),(["a","j","y"],4)]
-- > [(["a"],2),(["a"],4)]
relPermute :: [Int] -> [([a],b)] -> [([a],b)]
relPermute is = map (first permute)
  where permute as = map ((as !!) . pred) is



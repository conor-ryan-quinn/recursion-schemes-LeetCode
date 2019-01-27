import Data.Functor.Foldable
import Data.Foldable
import Data.List
import Data.Set

{-
	Given an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0? Find all unique triplets in the array which gives the sum of zero.
-}

getFirst (x, _, _, _) = x

getSecond (_, x, _, _) = x

getThird (_, _, x, _) = x

getFourth (_, _, _, x) = x

threeSum :: [Int] -> Set [Int]
threeSum xs = fromList $ cata algebra xs
  where
    algebra Nil = []
    algebra (Cons next total) = total ++ checkNum (next, xs)

checkNum :: (Int, [Int]) -> [[Int]]
checkNum (n, xs) = getFirst (cata algebra xs)
  where 
    algebra Nil = ([], 0, 0, (length xs) - 1)
    algebra (Cons _ total) = case (summed == 0 && unique) of
      True -> ((sort [xs !! left, xs !! right, n]):list , cur + 1, left, right - 1)
      False -> case (summed < 0) of
        True -> (list, cur + 1, left + 1, right)
        False -> (list, cur + 1, left, right - 1)
      where
        left = getThird total
        right = getFourth total
        cur = getSecond total
        list = getFirst total
        summed = xs !! left + xs !! right + n
        unique = left /= right && left /= n && right /= n 

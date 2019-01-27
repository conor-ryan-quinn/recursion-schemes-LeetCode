import Data.Functor.Foldable
import Data.Foldable

{-
- Given n non-negative integers a1, a2, ..., an , where each represents a point at coordinate (i, ai). n vertical lines are drawn such that the two endpoints of line i is at (i, ai) and (i, 0). Find two lines, which together with x-axis forms a container, such that the container contains the most water.
-}

sumCata :: [Int] -> Int
sumCata = cata algebra
  where
    algebra Nil = 0
    algebra (Cons next total) = next + total

mulCata :: [Int] -> Int
mulCata = cata algebra
  where
    algebra Nil = 1
    algebra (Cons next total) = next * total

reverseCata :: [Int] -> [Int]
reverseCata = cata algebra
  where
    algebra Nil = []
    algebra (Cons next total) = total ++ [next]

buildList :: Int -> [Int]
buildList = ana coalg where
  coalg 0 = Nil
  coalg n = Cons n (n - 1)

getFirst (x, _, _) = x

getSecond (_, x, _) = x

getThird (_, _, x) = x

-- this is an anamorphism
mostWater :: [Int] -> Int
mostWater s = getFirst (cata algebra s)
  where
    algebra Nil = (0, 0, (length s) - 1)
    algebra (Cons _ total) = case (s !! left < s !! right) of
      True -> (max max_area cur_area, left + 1, right)
      False -> (max max_area cur_area, left, right - 1)
      where
        max_area = getFirst total
        cur_area = ((getSecond total) * (getThird total)) * (min (s !! (getSecond total)) (s !! (getThird total)))
        left = getSecond total
        right = getThird total

{-
mostWater :: [Int] -> Int
mostWater s = getFirst (ana coalgebra s)
  where
    coalgebra 0 = (0, 0, (length s) - 1)
    coalgebra (Cons next total) =
    -}

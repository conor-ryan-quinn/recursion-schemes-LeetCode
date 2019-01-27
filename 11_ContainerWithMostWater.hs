import Data.Functor.Foldable
import Data.Foldable

{-
- Given n non-negative integers a1, a2, ..., an , where each represents a point at coordinate (i, ai). n vertical lines are drawn such that the two endpoints of line i is at (i, ai) and (i, 0). Find two lines, which together with x-axis forms a container, such that the container contains the most water.
-}

getFirst (x, _, _) = x

getSecond (_, x, _) = x

getThird (_, _, x) = x

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

import Data.Functor.Foldable
import Data.Foldable

{-
- Given two lists where digits are stored in reverse order, add the two numbers and return a list
-}

reverseAdd :: [Int] -> [Int] -> [Int]
reverseAdd x y = cata algebra (zip x y)
  where
    algebra Nil = []
    algebra (Cons next total) = total ++ [fst next + snd next]

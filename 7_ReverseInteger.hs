{-# LANGUAGE FlexibleContexts #-}

import Data.Functor.Foldable
import Data.Foldable

{-
- Reverse the digits of a signed integer
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

reverseInteger :: Int -> Int
reverseInteger x = read (cata algebra $ show x)
  where 
    algebra Nil = []
    algebra (Cons next rest) =
      case (next == '-') of
        True -> [next] ++ rest
        False -> rest ++ [next]

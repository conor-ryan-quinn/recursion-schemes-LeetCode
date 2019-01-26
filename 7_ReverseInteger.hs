{-# LANGUAGE FlexibleContexts #-}

import Data.Functor.Foldable
import Data.Foldable

{-
- Reverse the digits of a signed integer
-}

reverseInteger :: Int -> Int
reverseInteger x = read (cata algebra $ show x)
  where 
    algebra Nil = []
    algebra (Cons next rest) =
      case (next == '-') of
        True -> [next] ++ rest
        False -> rest ++ [next]

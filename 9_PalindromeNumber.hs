{-# LANGUAGE FlexibleContexts #-}

import Data.Functor.Foldable
import Data.Foldable

{-
- Determine whether an Integer is a Palindrome
-}

isPalindrome :: Int -> Bool
isPalindrome s = cata algebra $ show s
  where
    algebra _ = (reverse (show s) == show s)

import Data.Functor.Foldable
import Data.Foldable

import qualified Data.HashMap.Strict as M

{-
- Given a string, find the length of the longest substring without repeated characters
-}

getFirst (x, _, _, _) = x

getSecond (_, x, _, _) = x

getThird (_, _, x, _) = x

getFourth (_, _, _, x) = x

longestSubstring :: String -> Int
longestSubstring s = getFirst (cata algebra s)
  where
    algebra Nil = (0, 0, 0, M.empty)
    algebra (Cons next total) =
      case (M.lookup next hash) of
        Nothing -> ((max largest (i - start + 1)), start, i + 1, (M.insert next i hash))
        (Just loc) -> case (start <= loc) of
          True -> (largest, loc  + 1, i + 1, (M.insert next i hash)) 
          False -> ((max largest (i - start + 1)), start, i + 1, (M.insert next i hash)) 
        where
          largest = getFirst (total)
          start = getSecond (total)
          i = getThird (total)
          hash = getFourth (total)

-- Austin Clyde
-- Lecture 16

import Control.Category hiding ((.), id)
import System.IO

-- 16.1*
join' :: Monad m => m (m a) -> m a
join' =  (>>= id)

{-
>>= :: m a -> (a -> m b) -> m b
>>= :: m a -> (a -> a) -> a
>>= :: m (m a) -> (m a -> m a) -> m a
-}

-- 16.2*
main :: IO ()
main = interact addLineNums

addLineNums str = unlines $ zipWith (++) linenums (lines str) where
  spaces   = 1 + $ length (lines str) `div` 10
  linenums = zipWith (++) (map show [1..]) (cycle ["." ++ take spaces (cycle " ")])

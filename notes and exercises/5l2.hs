-- Austin Clyde
-- Lecture 14

import Control.Applicative
import Data.Monoid

-- 14.1*
liftAN :: Applicative f => ([a] -> b) -> f [a] -> f b
liftAN = (<$>)

— test
> liftAN length (Just [1 ,2, 3, 4, 5])
Just 5


-- 14.3*
{-
> [(+),(*)] <*> pure 2 <*> pure 3
[5,6]
> ZipList [(+),(*)] <*> pure 2 <*> pure 3
ZipList {getZipList = [5,6]}

ZipList rewraps a list into a type ZipList a <=> [a]. So when
pure 2 is called in the second pattern, it produces an infinite list of 2's
that gets wrapped into a ZipList a type. This is different from the first
pattern where the application happens inside of lists.

ex)
  [(+), (-)] <*> pure 2 :: [a -> a]
which is just a list of functions while
  [(+), (-)] <*> pure 2 :: ZipList (a -> a)
somehide conceals the fact that it actually contains
a list as well. 

-}

-- 14.5*
altconcat :: Alternative f => [f a] -> f a
altconcat = foldr (<|>) empty

—- tests
> altconcat [[1, 2, 3], [], [2]]
[1,2,3,2]
> altconcat [Nothing, Just 2]
Just 2


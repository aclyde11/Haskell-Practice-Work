-- Austin Clyde
-- Lecture 3 Exercises

-- 3.1*
-- Integer can be found under GHC, so GHC.Integer.

-- 3.4*
-- (^2) :: Num a => a -> a
-- map :: (a -> b) -> [a] -> [b]
-- (a and b can be the same type)

--3.2, 3.3
-- (,,) String Int Int
--type Standing = [] ((,,) String Int Int)

module Rational where

import Prelude hiding (Rational)

data Rational = Rational Integer Integer
    deriving (Show)

oneHalf :: Rational
oneHalf = Rational (-2) (-4)

lowestTerms (Rational n d) =
  Rational (signum d  * div n divisor) (div (abs d) divisor)
  where divisor = gcd n d
  

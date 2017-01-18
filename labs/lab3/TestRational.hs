-- Austin Clyde

import Lab3Rational
import Data.List

-- Parse/rpn and eval has not changed so I won't test it

-- Frac operations
instance Eq Frac where
  Frac a b == Frac c d = a*d == c*d

test1 = Frac 1 1 + Frac 1 1 == Frac 2 1
test2 = Frac 0 1 + Frac 1 1 == Frac 1 1
test3 = Frac (-1) (-1) + Frac 0 1 == Frac 1 1
test4 = Frac (-1) 1 + Frac 0 1 == Frac (-1) 1

test5 = Frac 1 1 * Frac 1 1 == Frac 1 1
test6 = Frac (-1) 1 * Frac (-1) (-1) == Frac (-1) 1
test7 = Frac 0 (-1) * Frac 10 (-1) == Frac 0 1

tests = [ test1,
          test2,
          test3,
          test4,
          test5,
          test6,
          test7
        ]

testAll = and tests
testPasses = sum $ map fromEnum tests
testFirstFailIndex = elemIndex False tests

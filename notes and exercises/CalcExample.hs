import Calc

square :: Calculation
square = do
    kDup
    kMul

hypotenuse :: Calculation
hypotenuse = do
    square
    kSwap
    square
    kAdd
    kSqrt

test :: Double
test = perform $ do
    kEnter 1
    kEnter 2
    kAdd
    kEnter 3
    kMul    

test2 :: Double
test2 = perform $ do
  kEnter 1
  kEnter 1
  kAdd
  kSto
  kEnter 2
  kEnter 2
  kMul
  kRcl
  kMul

test3 = perform $ do
  kEnter 1
  kEnter 2
  kAdd
  kSto
  kEnter 9
  kRcl
  kDiv

{-
*Main> test
9.0
*Main> test2
8.0
*Main> test3
3.0
-}

-- Class 7
-- 10/10/16
 
{-
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  -- Defai;t inplementation
  x == y = not (x /= y)
  x /= y = not (x == y)

-}

data IntList = Empty | NonEmpty Int IntList

instance Eq IntList where
  Empty         == Empty         = True
  NonEmpty x xs == NonEmpty y ys = x == y && xs == ys
  _             == _             = False


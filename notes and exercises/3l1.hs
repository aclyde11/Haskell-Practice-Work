-- Austin Clyde
-- Lecture 7

-- 7.1*
data Pair x y = Pair x y
  deriving (Eq)

instance  (Ord a,  Ord b) => Ord  (Pair a b) where
  compare (Pair x1 y1) (Pair x2 y2) =
    if (x1 /= x2)
    then compare x1 x2
    else compare y1 y2

-- 7.2
instance (Show a, Show b) => Show (Pair a b) where
  show (Pair x y) = show x ++ " ," ++ show y

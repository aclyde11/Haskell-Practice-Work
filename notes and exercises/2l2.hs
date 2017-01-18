-- Austin Clyde
-- Lecture 5

-- 5.2*
signums x = case compare x 0 of
  LT -> -1
  EQ -> 0
  GT -> 1

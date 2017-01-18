-- monads

{-
instance Monad [] where
  xs >>= f = concatMap f xs
-}


foo :: [(Int, Int)]
foo = do
  x <- [1..10] -- [1..10] >>= \x ->
  y <- [1..10] -- [1..10] >>= \y ->
  return (x, y) -- [(x, y)]



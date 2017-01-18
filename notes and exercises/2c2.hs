-- Class 4

import Prelude hiding (foldr, foldl, length)

-- where acc is the inital value with the function

foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f x acc) xs

-- length = foldr (\_ -> (+) 1) 0
-- he says he likes this tho:
length = foldr (\_ acc -> 1 + acc) 0

-- (+1) === \x -> x + 1
-- (1+) === \x -> 1 + x
-- (f $) === \x -> f $ x
-- ($ x) === \x -> x $ x

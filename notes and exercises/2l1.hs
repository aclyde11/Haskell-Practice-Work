-- Austin Clyde
-- Lecture 4
import Prelude hiding (foldr, product, map, lookup)

-- 4.2*
product :: Num a => [a] -> a
product = foldr (*) 1
-- *Main> product [0]
-- 0
-- *Main> product [1, 2, 3]
-- 6

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []
-- *Main> map (++[3]) [[1, 2],[3, 4],[5, 6]]
-- [[1,2,3],[3,4,3],[5,6,3]]
-- *Main> map (+1) [1, 2, 3, 4]
-- [2,3,4,5]

--4.4*
type Bindings = [(String, Int)]

lookup :: String -> Bindings -> Int
lookup _ [] = error "value: key is not bound."
lookup key ((k, v) : bindings)
  | k == key = v
  | otherwise = lookup key bindings

bind :: String -> Int -> Bindings -> Bindings
bind key value [] = (key, value) : [] 
bind key value ((k, v) : bindings)
  | k == key = (key, value) : bindings
  | otherwise = (k, v) : (bind key value bindings)

-- *Main> let ages = [("Susan", 12), ("Austin", 19)]
-- *Main> lookup "Susan" ages
-- 12
-- *Main> lookup "Austin" ages
-- 19
-- *Main> bind "Yeju" 21 ages
-- [("Susan",12),("Austin",19),("Yeju",21)]
-- *Main> bind "Joe" 19 []
-- [("Joe",19)]





-- 4.3
concat1 :: [[a]] -> [a]
concat1 [] = [] 
concat1 (xs : xss) = xs ++ concat1 xss

concat2 :: [[a]] -> [a]
concat2 = foldr (++) [] 


--
foldr f i [] = i
foldr f i (x:xs) = f x (foldr f i xs)

sum = foldr (+) 0

filter p = foldr p' [] where
  p' x xs = if p x then x:xs else xs

--4.1
filter_guard p = foldr p' [] where
  p' x xs
    |p x = x:xs
    |otherwise = xs


{-
data Bindings = None
              | Bind String Int Bindings

lookup :: String -> Bindings -> Int
lookup _ None = error "value: key is not bound."
lookup key (Bind k v bs)
  | k == key = v
  | otherwise = lookup key bs

bind :: String -> Int -> Bindings -> Bindings
bind key value None = Bind key value None
bind key value (Bind k v bs)
  | k == key = Bind key value bs
  | otherwise = Bind k v (bind key value bs)
-}

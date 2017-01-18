-- Austin Clyde
-- Lecture 11
import Data.Char
-- 11.1*
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just x) = Just (f x)

mapMaybes :: (a -> b) -> [Maybe a] -> [Maybe b]
mapMaybes = map . mapMaybe

mapLists :: (a -> b) -> [[a]] -> [[b]]
mapLists = map . map

-- 11.4*
fmapList1     :: Functor f => (a -> b) -> [f a] -> [f b]
fmapList1      = map . fmap

fmapList2     :: Functor f => (a -> b) -> [f a] -> [f b]
fmapList2 _ []     = []
fmapList2 g (x:xs) = fmap g x : fmapList2 g xs  


-- 11.2
{-
instance Functor Maybe where
--  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap f Nothing = Nothing
  fmap f (Just a) = Just a

--11.3

instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap f []     = []
  fmap f (x:xs) = f x : f xs
-}  

-- Lecture 14

import Prelude hiding (Applicative, (<*>), pure)

-- Last time we saw how foldable walks and does stuff
-- Can use foldMap <=> Foldr


-- Applicative
-- But what if we want to fmap (+) (Just a) (Just b)?
-- fmap :: (a -> b) -> f a -> f b
-- but we want
-- ???? :: (a -> b -> c) -> f a -> f b -> f c

class Functor f => Applicative f where
  (<*>)  :: f (a -> b ) -> f a -> f b
  pure :: a -> f a

-- So we can do this !
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just f) (Just x) = Just $ f x
applyMaybe _        _        = Nothing

-- Or the more general case of Applicavtive

instance Applicative Maybe where
  (<*>) = applyMaybe
  pure  = Just

-- So to add Just 1 and Just 2
-- pure (+) <*> Just 1 <*> Just 2

instance Applicative [] where
--(<*>) ::  [(a -> b)] ->  [a] - > [b]
  --(<*>) fs xs = concatMap (\f-> map f xs) fs
  (<*>) fs xs = [ f x | f <- fs, x <- xs]
  pure f  = [f]

newtype ZipList a = ZipList [a] deriving Show

instance Functor ZipList where
  fmap = fmap

instance Applicative ZipList where
  (<*>) (ZipList fs) (ZipList xs)  = ZipList $ zipWith ($) fs xs
  pure x = ZipList $ repeat x

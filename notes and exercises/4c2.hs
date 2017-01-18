-- Lecture 11
-- ugh
import Prelude hiding (Functor)
addOne :: Maybe Int -> Maybe Int
addOne Nothing = Nothing
addOne (Just a) = Just (a +1)

square :: Maybe Int -> Maybe Int
square Nothing = Nothing
square (Just n) = Just (n*n)

-- But oh no repeating things. Ppl don't like that

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just n) = Just (f n)

-- but that is dumb. Let's just use fmap

-- While we don't consider types to have types,
-- Bool :: *. * is a grounded type. Whenever
-- We have a type that just created.
-- Grounded: n Haskell, only grounded types are
-- finhabited by expressions. Conversely, all
-- Haskell expressions e have types t that have kind *

-- :k [] :: * -> *
-- Types that don't have type * are types that take
-- arguments. They take certain number of types are
-- and returns a new type.
-- Like (,) :: * -> * -> * ->
-- For instnace,
{-
class Eq (a :: *) where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
-}
-- You cannot define Eq on type * -> *. The only kind
-- of a can be *
-- f:: * -> *
class Functor f  where
  fmap :: (a -> b) -> f a -> f b

-- Here, f is not actually a type that
-- is usable. It is a type that is not
-- Fully applied.

-- Just as Eq has some rules, so does functor.
-- (1) fmap id x = id x
-- (2) fmap (g . f) x = fmap g (fmap f x)
instance Functor Maybe where
  --fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap foo Nothing  = Nothing
  fmap foo (Just a) = Just (foo a)

instance Functor ((,) first) where
  --fmap :: (a -> b) -> (first, a) -> (first, b)
  fmap foo (a, b) = (a, foo b)


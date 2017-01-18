-- Lecture 13

import Prelude hiding (Foldable, foldr, foldMap)

-- A monoid takes a set, an operation, and the id element
-- and satifisfies the following properties
-- (1) Associativity
--       (a op b) op c = a op (b op c)
-- (2) a op id = a
-- (3) id op a = a

{-
class Monoid m where
  --id :: m
  --op :: m -> m -> m
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat ms = foldr mappend mempty ms
  
instance Monoid [t] where
  mempty = []
  mappend = (++)
  
newtype Sum a = Sum { getSum :: a} deriving Show
newtype Product a = Product { getProduct :: a} deriving Show

instance (Num a) => Monoid (Sum a)  where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum $ x + y

instance (Num a) => Monoid (Product a) where
  mempty = Product 0
  mappend (Product x) (Product y) = Product $ x * y

instance (Monoid a) => Monoid (Maybe a) where
  mempty = Nothing

  mappend Nothing Nothing   = Nothing
  mappend Nothing (Just y)  = Just y
  mappend (Just x) Nothing  = Just x
  mappend (Just x) (Just y) = Just $ mappend x y

-}

-- Instead of being attached to the list type
-- We can make it more general from
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- And the kind of t is * -> *
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
  foldMap foo dataStruct = foldr (mappend . foo)  mempty dataStruct
-- We can actually define these in terms of each other

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving Show

instance Foldable Tree where
--foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f init (Leaf x) = f x init
  foldr f init (Node left right) = foldr f (foldr f init right) left 

--foldMapp :: (Monoid m) => (a -> m) -> t a -> m
  foldMap f (Leaf x) = f x
  foldMap f (Node left right) = mappend (foldMap f left) (foldMap f right) 

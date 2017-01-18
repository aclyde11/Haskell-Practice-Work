-- Austin Clyde
-- Lecture 12
import Data.Char
-- 12.1*
data BinaryTree a =
    Leaf a
  | Node (BinaryTree a) (BinaryTree a)
  deriving (Show)

instance Functor BinaryTree where
--fmap :: (a -> b) -> BinaryTree a -> BinaryTree b
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

-- 12.4*
{-
instance Functor ((->) a) where
  fmap :: (b -> c) -> ((->) a b) -> ((->) a c)
  -- demonstrating that functions are sets of tuples
  -- (a, b) \in f so the fmap of the pair is
  -- (a, b) -> (a, c), as we know from tuples.
  -- With sugar, can be written clearly as
  fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)
-}


-- 12.2



-- 12.3
newtype Compose g f a = Compose { runCompose :: g (f a)}

instance (Functor g, Functor f) => Functor (Compose g f) where
--fmap :: (a -> b) -> Compose g f a -> Compose g f b
  fmap h = Compose . fmap (fmap h) . runCompose

-- Austin Clyde Lecture 13
import Data.Foldable
import Data.Monoid

-- 13.4*
instance Foldable [] where
--foldMap :: Monoid m => (a -> m) -> [a] -> m
  foldMap _ []     = mempty
  foldMap f (x:xs) = f x <> foldMap f xs 

*Main> foldMap (Sum) []
Sum {getSum = 0}
*Main> foldMap Sum [1, 2, 3]
Sum {getSum = 6}


instance Foldable Maybe where
  foldMap _ Nothing  = mempty
  foldMap f (Just x) = f x

*Main> foldMap Product (Just 3)
Product {getProduct = 3}
*Main> foldMap Product (Nothing)
Product {getProduct = 1}


instance Foldable (Either a) where
  foldMap _ (Left _)  =  mempty
  foldMap f (Right x) = f x

*Main> foldMap (++ "aa") (Left 3)
""
*Main> foldMap (++ "aa") (Right "hi")
"hiaa"


instance Foldable ((,) a) where
  foldMap f (_, b) = f b

*Main> foldMap (++ "!") (3, "hey")
"hey!"

-- 13.5*
newtype FstPair a b = FstPair { getFstPair :: (b, a) }
instance Foldable (FstPair a) where
  foldMap f (FstPair (b, _)) = f b

-- tests
{-
*Main> foldMap (++ "!") (FstPair ("hello", "greetings"))
"hello!"
*Main> foldMap Sum (FstPair (0,1))
Sum {getSum = 0}
-}




--



data BinaryTree a
  = Leaf a
  | Node (BinaryTree a) (BinaryTree a)
  deriving Show


instance Foldable BinaryTree where
  foldMap f (Leaf x)   = f x
  foldMap f (Node l r) = foldMap f l <>  foldMap f r 


--if we provied a functor instance:
{-
instance Foldable BinaryTree where
  foldMap f = fold . fmap f
  fold (Leaf x) = x
  fold (Node l r) = fold l <> fold r
-}

-- 13.1
-- (:) and [] because : isn't assoc


-- 13.2
{-
newtype First a = First { getFirst :: Maybe a} deriving Show
newtype Last a  = Last  { getLast  :: Maybe a} deriving Show

instance Monoid (First a) where
  mempty  = First Nothing
  mappend x@(First (Just _))  _ = x
  mappend _                   _ = mempty

-}
-- 13.3


-- 13.6
-- toList = foldr (:) [] 



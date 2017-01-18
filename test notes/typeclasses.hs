import Data.Monoid
import Control.Applicative


data List a
  = Empty
  | Node a (List a) 
  deriving (Show, Eq)

instance Monoid (List a) where
  mempty      = Empty
  mappend a Empty        = a
  mappend Empty b        = b
  mappend (Node a as) b  = Node a (mappend as b)

instance Functor List where
  fmap f Empty       = Empty
  fmap f (Node a as) = Node (f a) (fmap f as)

instance Applicative List where
  pure a             = Node a Empty
  Empty       <*> _  = Empty
  (Node f fs) <*> as = fmap f as  `mappend` (fs <*> as)

instance Monad List where
  return = pure
  Empty >>= f       = Empty
  (Node a as) >>= f = f a `mappend` (as >>= f)

instance Foldable List where
  foldMap f Empty       = mempty
  foldMap f (Node a as) = f a `mappend` foldMap f as

instance Alternative List  where
  empty                = Empty
  Empty          <|> b = b
  (Node a Empty) <|> b = Node a b 
  (Node a as)    <|> b = as <|> b  

data May a
  = None
  | One a

instance Functor May where
  fmap f None    = None
  fmap f (One a) = One (f a )

instance Applicative May where
  pure          = One
  None <*> _    = None
  (One f) <*> b = fmap f b

instance Monad May where
  return        = pure
  None >>= _    = None
  (One a) >>= f = f a

instance Alternative May where
  empty = None
  None <|> b  = b
  One a <|> _ = One a

instance (Monoid a) => Monoid (May a) where
  mempty  = None
  mappend None b = b
  mappend a None = a
  mappend (One a) (One b) = One (a `mappend` b)

instance Foldable May where
  foldMap f None    = mempty
  foldMap f (One a) = f a

{-
newtype First a = First { getFirst :: Maybe a }
newtype Last  a = Last  { getLast  :: Maybe a }

instance Monoid (First a) where
  mempty      = First Nothing
  mappend a b = First $ getFirst a <|> getFirst b

instance Monoid (Last a) where
  mempty      = Last Nothing
  mappend a b = Last $ getLast b <|> getLast a
-}

data LR a b
  = L a
  | R b

instance Functor (LR a) where
  fmap _ (L a) = L a
  fmap f (R b) = R (f b)

instance Applicative (LR a) where
  pure = R
  (Left e) <*> _  = Left e
  (Right f) <*> r = fmap f r

instance Monad (LR a) where
  return = pure
  (Left e)  >>= _ = Left e
  (Right b) >>= f = f b


instance Foldable (LR a) where
  foldMap f (L _) = mempty
  foldMap f (R a) = f a

data  Pair a b = Pair a b
instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

data FstPair b a = FstPair (a, b)
instance Foldable (FstPair b) where
  foldMap f (FstPair (a,_)) = f a

data BinTree a
  = Leaf a
  | Branch (BinTree a) (BinTree a)

instance Foldable BinTree where
  foldMap f (Leaf a)   = f a
  foldMap f (Branch l r) = foldMap f l <> foldMap f r 

makeList :: (Foldable t) => t a -> [a]
makeList = foldr (:) []

altconcat :: Alternative f => [f a] -> f a
altconcat fs = foldr (<|>) empty fs 


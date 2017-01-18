-- Lecture 12
-- Functors

{-

newtype  Box a = Box { unbox :: a}

instance Functor Box where
--fmap :: (a -> b) -> Box a -> Box b
--fmap g (Box a) = Box (g a)
  fmap g = Box . g . unbox
-}

newtype AssocList k v = AssocList { unbox :: [(k, v)] }
instance Functor (AssocList k)  where
--fmap :: (a -> b) -> AssocList k a -> AssocList k b
  fmap f = AssocList . fmap (fmap f) . unbox 


instance Functor ((->) input) where
--fmap :: (a -> b) -> (((->) input) a) -> (((->) input) b)
--fmap :: (a -> b) -> (input -> a) -> (input b)
--fmap f g = \input -> f (g input)
  fmap f g = f . g
--fmap     = (.)

  

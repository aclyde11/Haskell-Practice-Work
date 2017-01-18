-- Lecture 15
-- Monads ;(

type Person = String
parent :: Person -> Maybe Person
parent p = undefined

grandparent :: Person -> Maybe Person
grandparent p =
  case parent p of
    Just par -> parent par
    Nothing  -> Nothing

{-
greatgrandparent :: Person -> Maybe Person
greatgrandparent p =
  case parent p of
    Just par -> grandparent par
    Nothing  -> Nothing

-- What is the pattern here?
--bindAndApply :: (Person -> Maybe Person) -> Maybe Person -> Maybe Person
bindAndApply f (Just p)  = f p
bindAndApply _ (Nothing) = Nothing

mapMaybe     :: (a -> b) -> Maybe a -> Maybe b
applyMaybe   :: Maybe (Person -> Person) -> Maybe Person -> Maybe Person
bindAndApply :: (Person -> Maybe Person) -> Maybe Person -> Maybe Person
-}
-- more generallly
bindAndApply :: (a -> Maybe b) -> Maybe a -> Maybe b
bindAndApply f (Just p)  = f p
bindAndApply _ (Nothing) = Nothing

greatgrandparent p = bindAndApply parent (bindAndApply parent (Just p))

--......
-- MONAD
--......

{-
class Applicative f => Monnad f where
--fmap  :: (a -> b) -> f a -> f b
--(<*>) :: f (a -> b) -> f a -> f b
--pure  :: a -> f a
  -- bind: sequences functions one after another
  -- where each can depend on what happens after
  (>>=) :: f a -> (a -> f b) -> f b
-}


-- Basically an applicative functor that
-- allows sequencing
instance Monad Maybe where
--(>>=) :: f a -> (a -> f b) -> f b
  Just a  >>= f = f a
  Nothing >>= _ = Nothing

-- so now we can do this:
greatgrandparent' p =
  -- (Just p) >>= parent >>= parent
  do
    p'   <- Just p     -- Just p    >>= \p'  ->
    p''  <- parent p'  -- parent p' >>= \p'' ->
    parent p''         -- parent p''
-- here each p :: Parent


-- do is just syntactic sugar for calling bind
-- and for strining together monads ;(

nthParent :: Int -> Person -> Maybe Person
nthParent 1 p = parent p 
nthParent n p = parent p >>= nthParent (n-1)



-- Austin Clyde
-- Lecture 15

-- 15.1*
{-
Left identity:
return a >>= f
pure   a >>= f
Just a   >>= f
f a
=> so return >>= f <=> f

Right identity:
m a    >>= return
(m a can be either Nothing or Just a) 
Just a >>= return    || Nothing >>= return
return a             || Nothing
Just a
=> so m a >>= return <=> m a 

Associativity:
(Just a >>= f) >>= g
f a >>= g <=>
(\p -> f p >>= g) $ a <=>
Just a >>= (\p -> f p >>= g)

-- 15.3*
Since only Right of Either can be altered (since a Monad instance has to be * -> *),
there is no fail value within the Monad--which is why it cannot be a Monad plus (also there is no Alternative instance for Either since applicative functors have no binary operator as there is no reaosnable way to Left a <|> Right b <|> Right b)

-}

-- 15.2
{-
instance Monad (Either a) where
  Left a  <<= _  = Left a
  Right a <<= f = f a 
-}

-- 15.4

-- Austin Clyde
-- Lecture 18


-- 18.2*
-- Draw diagram for (>>=)
-- sa >>= f = State $ /s ->
       -- let (a, s') = rimState sa s
       --     sb      = f a
       -- in runState sb s' 
















--
-- 18.3*
instance Monad (State s) where
  return a = State $ \s -> (a, s)
  ma >>= f = State $ \s ->
    let (a, t) = runState ma s
        mb     = f a
        (b, u) = runState mb t
    in  (b, u)

ma >>= f = State $ \s ->
  let (a, t) = runState ma s
      mb     = f a in 
      runState mb t

ma >>= f = State $ \s -> 
  (\(a, s') -> let mb = f a in runState mb t) $ runState ma s

ma >>= f = State $ (\(a, s) -> runState (f a) s) . runState ma 

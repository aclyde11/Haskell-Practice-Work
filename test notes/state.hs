-- My own instance of State

newtype State b a = State { runState :: b -> (a, b) }

instance Functor (State b) where
  fmap f st = State $ \s ->
                        let (a, s') = runState st s in
                          (f a, s')

instance Applicative (State b) where
  pure a     = State $ (,) a
  sFs <*> st = State $ \s ->
                         let (a, s') = runState st  s
                             (f, _ ) = runState sFs s
                         in (f a, s')

instance Monad (State b) where
  return   = pure
  -- st >>= f = State $ \s ->
  --                      let (a, s')  = runState st s in
  --                        runState (f a) s'
  st >>= f = State $ (\(a,s) -> runState (f a) s) . runState st

get :: State s s
get = State $ \s -> (s,s)
    
put :: s -> State s ()
put t = State $ \s -> ((),t)
    
modify :: (s -> s) -> State s ()
modify f = State $ (,) () . f

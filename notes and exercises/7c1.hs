-- Lecture 18

import Control.Arrow
-- ex) stacks

{-
coolComputation :: Stack -> (Int, Stack)
coolComputation s0 =
  let s1 = push 1 s0 in
  let s2 = push 2 s1 in
  let s3 = push 3 s2 in
  let (a, a4) = pop s4 in
  .....
-}

-- so state can become a monad !
newtype State s a = State { runState :: (s -> (a, s)) }
-- which is really just an alis but a haskell quirk
-- type State s a = s -> (a, s)

instance Functor (State s) where
--fmap :: (a -> b) -> State s a -> State s b
  fmap f stck = State $ \initState ->
    let (a, updatedState) = runState stck initState in
      (f a, updatedState)

instance Applicative (State s) where
 -- pure :: a -> State s a
   pure a = State $ \s -> (a, s)
 -- <*> :: State s (a -> b) -> State s a -> State s b
   sf <*> sa = State $ \s ->
               let (f, s') = runState sf s in
               let (a, s'') = runState sa s' in
               (f a, s'')

instance Monad (State s) where
-- (>>=) :: State s a -> (a -> State s b) -> State s b
  sa >>= f = State $ \s ->
             let (a, s') = runState sa s in
             let sb      = f a in
             runState sb s'

type Stack = [Int]

pop  :: State Stack Int
pop  = State $ \(x:xs) -> (x, xs)

push   :: Int -> State Stack ()
push i = State $ \stk -> ((), i:stk)

coolComputation :: State Stack Int
coolComputation s0 = do
  push 1
  push 2
  push 3
  a <- pop
  b <- pop
  push (a + b)
  pop
--  e1 :: State Stack t1
--  e2 :: State Stack t2
-- ...
  
               

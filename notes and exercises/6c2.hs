-- Class 16

type Person = String

parent :: Person -> Maybe Person
parent p = undefined

sibling :: Person -> Person -> Bool
{-
subling p1 p2 = do
  pp1 <- (return p1 >>= parent)
-}

sibling p1 p2 =
  case foo of {Just True -> True; _ -> False}
  where foo = do
          pp1 <- parent p1
          pp2 <- parent p2
          reutrn $ p1 /= p2 && pp1 == pp2
{-
    (return p1 >>= parent) >>= \pp1 ->
    (return p2 >>= parent) >>= \pp2 ->
    return $ p1 /= p2 && p1
-}

-- 'True = ()"
-- 'False = Nothing"
boolToMaybe :: Bool -> Maybe
boolToMaybe True = Just ()
boolToMaybe False = Nothing

sibling' :: Person -> Person -> Mayeb ()
sibling' p1 p2 = do
  pp1 <- parent p1
  pp2 <- parent p2
  boolToMaybe $ p1 /= p2 && pp1 == pp2


guard :: Alternative f => Bool -> f ()
guard True  = pure ()
guard False = mempry


{-
Monoid (S, id, op)
 -- mappend = op
 -- mempty = id

So if we want that for kind * -> *
Alternative (S, id, op)
 -- empty :: f a
 -- (<|>) :: f a -> f a -> f a

There is a super class for Alternative, Applicative
Functor
 -- fmap

Applicative
 -- (<*>) ::  
 -- pure  :: 


Monad is an applictive functor
-- >>=
-- return


Which is both a monad and a monoid
Okay so now thereis MonadPlus * -> *
which is an applictive
mzero :: f a
mplus :: f a -> f a -> f a 
-}

instance Monad [] where
-- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concatMap f xs
  (>>=) = flip concatMap

foo :: [(Int, Int)]
foo =
  [1..10] >>= \x -> (
    [1..10] >>= \y ->
        return (x,y)


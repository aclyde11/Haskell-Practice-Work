-- Lecture 21
import Text.ParserCombinators.ReadP
import Data.Char
{-
{-
instance Monad Parser where
  pa >>= f = Parser $ \s ->
    [ (b, s'') | (a, s')  <- runParser pa s,
                 (b, s'') <- runParser (f a) s' ]
-}

runThree :: (Monad m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
runThree f m1 m2 m3 =
  x1 <- m1
  x2 <- m2
  x3 <- m3
  return $ f x1 x2 x3

runThree :: (Applicative p) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
runThree f p1 p2 p3 = f <$> p1 <*> p2 <*> p3


-}

data List
  = Nil
  | Cons Int List

instance Show List where
--show a :: a -> String
  show Nil = "[]"
  show (Cons a as) = (show a) ++ ": " ++ show as

instance Read List where
  readsPrec _ = readP_to_S parseList

parseNil :: ReadP List
parseNil = do
  string "[]"
  return Nil

parseInt :: ReadP Int
parseInt = do
  s <- munch1 isDigit
  return (read s)

parseCons :: ReadP List
parseCons = do
  n <- parseInt
  char ':'
  ns <- parseList
  return $ Cons n ns

parseList = parseNil +++ parseCons

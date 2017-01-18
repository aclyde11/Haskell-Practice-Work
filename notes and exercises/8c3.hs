-- Lecture 23
-- Monad Transformer Library ;(

import Control.Monad
import Data.Char

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just

  (MaybeT mma)  >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a  -> runMaybeT $ f a

instance Monad m => Functor (MaybeT m) where
  fmap f x = pure f <*> x


instance Monad m => Applicative (MaybeT m) where
  pure  = return
  (<*>) = ap

liftMaybeT :: (Monad m) => m a -> MaybeT m a
liftMaybeT action = MaybeT $ do
  a <- action
  return $ Just a

maybeReadInt :: MaybeT IO Int
maybeReadInt = do
  s <- liftMaybeT getLine
  (if all isDigit s
    then return $ read s
    else MaybeT $ return Nothing)

maybeAddThree :: MaybeT IO Int
maybeAddThree = undefined


instance Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  action1 <|> action2 = do
    ma1 <- runMaybeT action1
    case ma1 of
      Nothing -> runMaybeT action2
      Just a  -> return ma1

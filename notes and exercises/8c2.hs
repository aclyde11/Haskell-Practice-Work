-- Monad transformers
-- Lecture 22

import Control.Applicative
import Control.Monad
import Data.Char

readInt :: IO (Maybe Int)
readInt = do
  s <- getLine
  if all isDigit s
    then return $ Just (read s)
    else return $ Nothing

addThree :: IO (Maybe Int)
addThree = do
  mi <- readInt
  mj <- readInt
  mk <- readInt
  return $ do
    i <- mi
    j <- mj
    k <- mk
    return $ i + j + k

bindFancy :: Monad m =>
             m (Maybe a)
          -> (a -> m (Maybe b))
          -> m (Maybe b)
bindFancy mma f = do
  ma <- mma
  case ma of
    Nothing -> return Nothing
    Just a  -> f a

addThree''' :: IO (Maybe Int)
addThree''' = 
  readInt `bindFancy` \i ->
  readInt `bindFancy` \j ->
  readInt `bindFancy` \k ->
    return $ Just $ i + j + k

-- MONAD TRANSFORMERS
-- Take a function M and add monad ability to ot

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just

  (MaybeT mma)  >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a  -> runMaybeT $ f a


-- Austin Clyde
-- Lecture 17

module Main where 
import Data.Char
import System.Environment
import System.Exit
import System.IO
import Control.Monad   
import Control.Category hiding ((.))

mUntil :: Monad m => m Bool -> m b -> m ()
mUntil control action = do
  terminate <- control
  if terminate
  then return ()
  else do
    action
    mUntil control action
    
usage :: IO ()
usage = do
  progname <- getProgName
  hPutStrLn stderr $ "usage: " ++ progname ++ " infile outfile"
  exitWith $ ExitFailure 255
    
main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile,outfile] -> do
              withFile' infile ReadMode $ \input ->
                withFile' outfile WriteMode $ \output ->
                    mWhileJust (hMaybeGetChar input) $
                        toUpper >>> hPutChar output
    _ -> usage


--17.1*
hMaybeGetChar   :: Handle -> IO (Maybe Char)
hMaybeGetChar h = 
  hIsEOF h >>= \empty ->
    if empty
    then (return Nothing)
    else (hGetChar h  >>= (return . Just))

mWhileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
mWhileJust control action = loop where
  loop = control >>= \maybe -> 
         case maybe of
           Just a -> (action a *> loop)
           _      -> return ()

-- 17.2
withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' infile iomode action = do
  handle <- openFile infile iomode
  r <- action handle
  hClose handle
  return r

{-
withFile2 infile iomode action =
  bracket (openFile infile iomode) hClose action
-}


readFile' :: FilePath -> IO String
readFile' path = withFile path ReadMode hGetContents

writeFile' :: FilePath -> String -> IO ()
writeFile' path  str = withFile path WriteMode $ \handle ->
                         hPutStr handle str

writeFile2 path str = do
  handle <- openFile  path WriteMode
  hPutStr handle str

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f sa = State $ \s -> let (a, s') = runState sa s in (f a, s')

instance Applicative (State s) where
  pure a    = State $ \s -> (a, s)
  fs <*> sa = State $ \s -> let (f, _)  = runState fs s
                                (a, s') = runState sa s
                            in (f a, s')

instance Monad (State s) where
  return = pure
  sa >>= f = State $\s -> let (a, s') = runState sa s in runState (f a) s' 

put :: s -> State s ()
put s' = State $\s -> ((),s)

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)


newtype Parser a = Parser { runParser :: String -> [(a, String)] }

satify :: (Char -> Bool) -> Parser Char
satify = Parser $ \s ->
  case s of
    _  -> []
    a:as -> 

char :: Char -> Parser Char
char = satify . (==)


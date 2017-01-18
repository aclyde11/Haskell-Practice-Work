-- Class 8

-- Going over the HW ord type class thing

-- Haskell IO
import System.Environment


login :: IO ()
login =
    do
      putStrLn "What's your name?"
      s <- getLine
      name <- getEnv "USER"
      if s == name
         then putStrLn "good job"
         else do
           putStrLn "Bad job"
           login 
-- let x = e
--     x :: t
--     e :: t

-- x <- e
-- e :: IO t
-- x :: t

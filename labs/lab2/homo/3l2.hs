-- Austin Clyde
-- Lecture 8

-- 8.1*
module Main where
import System.Environment
  main :: IO ()
  main =
    do
      putStrLn "Hello. I am a HAL 9000 series computer."
      name <- getEnv "USER"
      putStrLn $ "Good morning, " ++ (toUpper $ head name):(tail name)) ++ "."

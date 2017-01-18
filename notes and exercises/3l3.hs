-- Austin Clyde
-- Lecture 9

module Main where

import Data.Char
import System.Exit
import System.Environment
import System.IO
import Text.Regex.Posix

rot :: Int -> String -> String
rot = map . rotChar

rotChar :: Int -> Char -> Char
rotChar n c
    | isLower c = rotCase 'a' c
    | isUpper c = rotCase 'A' c
    | otherwise = c
  where rotCase base char = chr (ord base + (ord char - ord base + n) `mod` 26)

vigenere ::  String -> String -> String
vigenere pass' str = zipWith (rotChar) pass str where
  decipher   = head pass' == '-'
  pass       = map nextChar (cycle (if decipher then tail pass' else pass'))
  nextChar c = if decipher then (-1) * (ord c - 97) else ord c - 97

vigenereStdin :: String -> String -> IO ()
vigenereStdin x y = putStrLn $ vigenere x y

rotStdin :: Int -> String -> IO ()
rotStdin x y = putStrLn $ rot x y

usage :: IO()
usage = do
  progname <- getProgName
  hPutStrLn stderr $ "usage: " ++ progname ++ " [n]"
  exitWith $ ExitFailure 255

main :: IO ()
main = do
  args <- getArgs
  input <- getContents
  if all isAscii input
    then case args of
    [] -> rotStdin 13 input
    [x]
        | x =~ "^-?[0-9]+$" -> rotStdin (read x) input
        | x =~ "^-?[a-z]+$" -> vigenereStdin x input
        | otherwise         -> usage
    _  -> usage
    else usage


—- tests: 
austin@Austin's_Macbook:~/cmsc161/classes$ ./3l3 stuart <<<"Haskell is fun"
Ztmkved cs ymg
austin@Austin's_Macbook:~/cmsc161/classes$ ./3l3 -stuart <<<"Ztmkved cs ymg"
Haskell is fun
austin@Austin's_Macbook:~/cmsc161/classes$ ./3l3 3 <<<"hi how are you"
kl krz duh brx
austin@Austin's_Macbook:~/cmsc161/classes$ ./3l3 23 <<<"kl krz duh brx"
hi how are you



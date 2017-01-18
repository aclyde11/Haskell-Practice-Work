-- Austin Clyde

import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = interact wordCount

wordCount :: String -> String
wordCount = showCounts . countWords . splitWords . map toLower

splitWords = sort . wordsBy (not . isWord) where
  isWord = (`elem` concat [['a'..'z'], ['-', '\'']])

type WordList = [(String, Int)]

countWords :: [String] -> WordList
countWords = foldr addOne [] where 
  addOne word [] = [(word, 1)]
  addOne word ((k, v) : wordList)
    | k == word = (word, v + 1) : wordList
    | otherwise = (k, v) : (addOne word wordList)

showCounts :: WordList -> String
showCounts = foldl showListing "" where
  showListing rest (word, n) = word ++ " " ++ show(n) ++ "\n" ++ rest

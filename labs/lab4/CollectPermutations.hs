-- Austin Clyde

import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = interact wordPermutations

wordPermutations :: String -> String
wordPermutations = showPermutations . countWords . splitWords . map toLower

splitWords = wordsBy (not . isWord) where
  isWord = (`elem` concat [['a'..'z'], ['-', '\'']])

type PermList = [[String]]

isPerm :: String -> String -> Bool
isPerm p q = sort p == sort q

countWords :: [String] -> PermList
countWords list = filter (\x -> length x > 1) $ foldr addOne [] list where 
  addOne word [] = [[word]]
  addOne word (permGroup : wordList)
    | word `elem` permGroup = permGroup : wordList 
    | isPerm word (head permGroup) = (word :  permGroup) : wordList
    | otherwise     = permGroup : (addOne word wordList)

showPermutations :: PermList -> String
showPermutations list = unlines . sort . lines $ foldl combine "" list  where
  combine r words  = r ++ listPerm (sort words) ++ "\n"
  listPerm :: [[Char]] -> [Char]
  listPerm (x:[]) = x
  listPerm (x:xs) = x ++ ", " ++ listPerm xs

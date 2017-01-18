-- Austin Clyde
-- Lecture 21

import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char

-- CSV example (exercises below)

newline :: ReadP String
newline = string "\n" <++ string "\r\n" <++ string "\r"

simpleField :: ReadP String
simpleField = munch (`notElem` ",\"\n\r")

nextChar :: ReadP Char
nextChar = satisfy (/='"') <++ fmap (const '"') (string "\"\"")

quotedField :: ReadP String
quotedField = between (char '"') (char '"') (many nextChar)

complete :: ReadP a -> ReadP a
complete p = do
  result <- p
  peek <- look
  guard $ null peek || head peek `elem` ",\r\n"
  return result
    
field :: ReadP String
field = complete simpleField <++ complete quotedField

record :: ReadP [String]
record = field `sepBy1` char ','

csv :: ReadP [[String]]
csv = record `sepBy1` newline



parseWith :: ReadP a -> String -> a
parseWith p s = case [a | (a,t) <- readP_to_S p s, all isSpace t] of
                  [a] -> a
                  [] -> error "no parse"
                  _ -> error "ambiguous parse"

parseCSV :: String -> [[String]] 
parseCSV  = parseWith csv where
  csv = record `sepBy1` newline
  record = field `sepBy1` char ','
  field = complete simpleField <++ complete quotedField
  simpleField = munch (`notElem` ",\"\n\r")
  quotedField = between (char '"') (char '"') (many nextChar)
  nextChar = satisfy (/= '"') <++ fmap (const '"') (string "\"\"")
  complete ma = do
    result <- ma
    peek <- look
    guard $ null peek || head peek `elem` ",\r\n"
    return result
  newline = string "\n" <++ string "\n\r" <++ string "\r"
    
readCSV :: FilePath -> IO [[String]]
readCSV = fmap parseCSV . readFile

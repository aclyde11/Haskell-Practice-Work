-- Class 9
import Data.Char
main :: IO ()
main =
  interactWithUser "Enter your favrorite number." message

interactWithUser :: String -> (String -> String) -> IO ()
interactWithUser prompt f =
  do
    putStrLn prompt
    input <- getLine
    putStrLn (f input)
    interactWithUser prompt f

isInt :: String -> Maybe Int
isInt []  = Nothing
isInt ('-':s) = isInt s
isInt s
  | all isDigit s = Just (read s)
  | otherwise     = Nothing

message :: String -> String
message s = case isInt s of
  Nothing -> "Not a number."
  Just i  -> "Nice number: " ++ show (i + 1)

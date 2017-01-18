-- Lecture 20
import Data.Char

newtype Parser a = Parser { runParser :: (String -> [(a, String)]) }

char :: Char -> Parser Char
char c = satisfy (== c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s ->
  case s of
    (a:as) -> [ (a, as) | p a ]
    _      -> []

alpha = satisfy isAlpha

{-
string :: String -> Parser String
string prefix = Parser $ \s ->
  let (pre, suf) = splitAt (length prefix) s in
    if pre == prefix then [(pre, suf)]
                     else []
-}

string s = token s s

token :: String -> a -> Parser a
token prefix a = Parser $ \s ->
  let (pre, suf) = splitAt (length prefix) s in
    if pre == prefix then [(a, suf)]
                     else []

instance Functor Parser where
  fmap f pa = Parser $ \s ->
    [ (f a, t) | (a, t) <- runParser pa s ]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  pf <*> pa = Parser $ \s ->
    [ (f a, s'')  | (f, s') <- runParser pf s
                  , (a, s'') <- runParser pa s' ]

parseTrue = token "True" True
parseFalse = token "False" False

instance Alternative Parser where
  p1 <|> p2 = Parser $ \s ->
    runPArser p1 s ++ runPArser p2 s
  empty = Parser $ \_ -> []

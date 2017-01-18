-- Austin Clyde
-- Lecture 20
import Data.Char
import Parser
import Control.Applicative

-- 20.1*
{-
char :: Char -> Parser Char
char c = satisfy (c==)

Since (==) is commutative
char c = satisfy (==c)
char c = satisfy . (==) $ c
and eta reduce
char = satisfy . (==)
-}

-- 20.2*
data ComplexInt = ComplexInt Int Int deriving Show

instance Read ComplexInt where
  readsPrec _ = runParser parseComplex

parseInt :: Parser Int
parseInt   = read <$> some (satisfy isDigit) :: Parser Int

parseTuple :: Parser ()
parseTuple = const () <$> some (satisfy (`elem` [')', ',','(']))

parseComplexInt :: Parser ComplexInt
parseComplexInt = flip ComplexInt 0 <$> parseInt 

parseComplexPair :: Parser ComplexInt
parseComplexPair = do
  parseTuple
  a <- parseInt
  parseTuple
  b <- parseInt
  parseTuple
  return $ ComplexInt a b

parseComplex :: Parser ComplexInt
parseComplex = parseComplexPair <|> parseComplexInt

{- tests
*Main> read "12" :: ComplexInt
ComplexInt 12 0
*Main> read "(0,1)" :: ComplexInt
ComplexInt 0 1
-}

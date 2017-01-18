-- Write your parser in this file.

module Lab6 (
  Name,
  Number,
  TopLevelExp(..),
  MathExp(..),
  parse
) where

import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.


-- A top-level expression is either:
--
-- 1) A bare mathematical expression:
--
-- 4 + (2*5)
--
-- 2) A let-binding followed by an expression to evaluate:
--
-- let x = 5 in x + 4
--
-- let (x1, y1, x2, y2) = (5,5,10,10) in (y2-y1)*(y2-y1) + (x2-x1)*(x2-x1)
--
-- You can assume that the tuples on either side of the = sign are
-- always the same length--if not it will be treated as an eval error.

data TopLevelExp
  = MathTLE MathExp
  | LetTLE [Name] [MathExp] MathExp
  deriving (Eq, Show)


-- A math expression is a number, a variable,
-- a negation of a math expression, or any of
-- the four major operations plus power on a
-- pair of math expressions.
--
-- In the actual parser:
--   1. Precedence should be standard order of operations.
--   2. Negation should only precede a number, a variable, or
--      a parenthetical expression.
--   3. A variable starts with a lowercase letter and after
--      the first letter is any alphanumeric character a-z A-Z 0-9.
--
-- Your parser does _not_ need an explicit tokenization step like Lab 3.
-- In the functional parsing paradigm, tokenization+parsing occur
-- simultaneously.

data MathExp
  = Number Number
  | Var    String
  | Neg    MathExp
  | Plus   MathExp MathExp
  | Minus  MathExp MathExp
  | Mult   MathExp MathExp
  | Div    MathExp MathExp
  | Pow    MathExp MathExp
  deriving (Eq, Show)

parseOp :: Char -> ReadP (MathExp -> MathExp -> MathExp)
parseOp op = do
  skipSpaces
  c <- satisfy (==op)
  return $ case op of
             '+' -> Plus
             '-' -> Minus
             '*' -> Mult
             '/' -> Div
             '^' -> Pow

parseNeg :: ReadP MathExp -> ReadP MathExp
parseNeg parser = do
  skipSpaces
  neg <- option "" (string "-")
  case neg of
    "-" -> Neg <$> parser
    _   -> parser

isVarName :: Char -> Bool
isVarName c = or [isAlpha c, isDigit c]

parseMathExp :: ReadP MathExp
parseMathExp = parseStart where
  parseStart   = chainl1 parseMultDiv (parseOp '+' +++ parseOp '-')
  parseMultDiv = chainl1 parsePow     (parseOp '*' +++ parseOp '/')
  parsePow     = chainr1 parseValues  (parseOp '^')
  parseValues  = parseParens +++ parseNumber <++ parseVar
  parseNumber  = parseNeg $ Number . read <$> (skipSpaces *> munch1 isDigit)
  parseVar     = parseNeg $ Var <$> (skipSpaces *> munch1 isVarName)
  parseParens  = parseNeg $ between (skipSpaces *> char '(')
                                    (skipSpaces *> char ')')
                                     parseMathExp

parseMathTLE :: ReadP TopLevelExp
parseMathTLE = MathTLE <$> parseMathExp

parseOneOrList :: ReadP a -> ReadP [a]
parseOneOrList parser = do
  skipSpaces
  (pure <$> parser) <++ between (char '(') (char ')') (parseMany)
  where
    parseMany = sepBy1 (skipSpaces *> parser) (skipSpaces *> char ',')

parseLetTLE :: ReadP TopLevelExp
parseLetTLE = do
  skipSpaces *> string "let"
  names      <- parseOneOrList (munch1 isVarName)
  skipSpaces *> char '='
  bindings   <- parseOneOrList parseMathExp
  skipSpaces *> string "in"
  exp        <- parseMathExp
  return $ LetTLE names bindings exp

parseTLE :: ReadP TopLevelExp
parseTLE = do
  tle <- parseLetTLE +++ parseMathTLE
  skipSpaces
  return tle

-- Run the parser on a given string.
--
-- You should not modify this function. Grading may
-- look for the specific messages below.
parse :: String -> Either String TopLevelExp
parse str =
  case (completeParses, otherParses) of
    ([(result, "")], _  ) -> Right result  -- Only complete result.
    ([]            , [] ) -> Left $ "No parse."
    ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ (show leastRemaning)
    (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ (show completeParses)
  where
    parses = readP_to_S parseTLE str
    (completeParses, otherParses) =
        partition (\(_, remaining) -> remaining == "") parses
    leastRemaning = minimumBy (comparing length) . map snd $ otherParses

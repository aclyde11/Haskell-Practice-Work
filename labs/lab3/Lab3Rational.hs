-- Austin Clyde
module Lab3Rational(
  ArithExp(..),
  Token(..),
  Frac(..),
  parse,
  eval,
  tokenize,
) where

import Data.Char
import Data.List

data Frac = Frac Int Int 

instance Show Frac where
  show (Frac n d)
    | d == 1    = show n
    | otherwise = show n ++ "/" ++ show d

instance Num Frac where
  (Frac n d) + (Frac n' d') = lowestTerms $ Frac (n*d' + n'*d) (d*d')
  (Frac n d) * (Frac n' d')
    | d*d' == 0 = error "(Frac): Divide by zero"
    | otherwise = lowestTerms $ Frac (n*n') (d*d')
  abs (Frac n d)            = Frac (abs n) (abs d)
  signum (Frac n d)         = Frac (signum $ n*d) (abs d)      
  fromInteger n             = Frac (fromInteger n) 1
  negate (Frac n d)         = lowestTerms $ Frac (-1 * n) d 

lowestTerms (Frac n d) =
  Frac (signum d * div n divisor) (div (abs d) divisor)
  where  divisor = gcd n d

data ArithExp
  = Number Frac
  | Add    ArithExp ArithExp
  | Multi  ArithExp ArithExp
  | Div    ArithExp ArithExp
  deriving Show

eval :: ArithExp -> Frac
eval (Number n)  = n
eval (Add a b)   = eval a + eval b
eval (Multi a b) = eval a * eval b
eval (Div a b)   = eval a `divideFrac`  eval b
  where divideFrac a (Frac c d) = a * (Frac d c)  

-- Everything is the same as in Lab3.hs from this point on EXCEPT
-- cominber in parse now wraps ints in a fraction (Frac n 1)

data Token
  = Num Int
  | Plus
  | Star
  | Slash
  | LParen
  | RParen
  | Stop
  deriving (Eq, Show)

instance Ord Token where
  a `compare` b = compare (opPres a) (opPres b)
    where 
    opPres (Plus)   = 1
    opPres (Star)   = 2
    opPres (Slash)  = 2
    opPres (LParen) = 0
    opPres (RParen) = 3

tokenize :: [Char] -> [Token]
tokenize []         = [Stop]
tokenize (hd:tl) 
  | elem hd "+*/()" = getOp hd        : tokenize tl
  | isNum hd        = Num (read num)  : tokenize rest
  | otherwise       = tokenize tl
  where
    isNum c      = or [isDigit c, c=='-'] 
    (num, rest)  = span isNum (hd:tl)
    getOp c
      | c == '+' = Plus
      | c == '*' = Star
      | c == '/' = Slash
      | c == ')' = RParen
      | c == '(' = LParen


parse :: [Token] -> ArithExp
parse toks =
  head $ foldr combine []  sortedToks 
    where
    (_, _, sortedToks)            = rpn (toks, [], []) 
    combine (Num n) xs            = Number (Frac n 1)  : xs 
    combine Plus  (expB:expA:xs)  = Add expA expB      : xs
    combine Star  (expB:expA:xs)  = Multi expA expB    : xs
    combine Slash (expB:expA:xs)  = Div expA expB      : xs

-- Converts list of tokens to RPN with precedence.
-- Should be called with ([Tokens to be sorted], [], [])
-- Returns ([],[],[Tokens sorted]). 
rpn :: ([Token], [Token], [Token]) -> ([Token], [Token], [Token])
rpn (thisTok:tailToks, ops, output) = 
  case thisTok of
    Num m  -> rpn (tailToks, ops, thisTok:output)
    LParen -> rpn (tailToks, LParen:ops, output)
    RParen -> case ops of
               LParen:rest -> rpn (tailToks, rest, output)
               popOp:rest  -> rpn (RParen:tailToks, rest, popOp:output)
    Stop   -> case ops of
               []          -> ([], [], output)
               popOp:rest  -> rpn ([Stop], rest, (popOp:output))
    _      -> case ops of
               []          -> rpn (tailToks, thisTok:ops, output)
               popOp:rest  -> if thisTok <= popOp --do the op with > precedence
                                then rpn (thisTok:tailToks, rest, popOp:output) 
                                else rpn (tailToks, thisTok:ops, output)

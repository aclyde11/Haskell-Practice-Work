

{-
-- Given a list of tokens, the list is sorted into reverse polish notation
sortOps :: ([Token], [Token], [Token]) -> ([Token], [Token], [Token])
sortOps ([], ops, left) = ([], ops, left)
sortOps ((nextTok:toksLeft), ops, output) = 
  case nextTok of
    Num m ->sortOps  (toksLeft, ops, nextTok:output)
    Plus  -> case ops of
               []            -> sortOps (toksLeft, Plus:ops, output)
               (nextOp:rest) -> if (opPres Plus) <= (opPres nextOp)
                                then  sortOps ((Plus:toksLeft), rest, nextOp:output)
                                else  sortOps (toksLeft, Plus:ops, output)
    Star  -> case ops of
               []            ->  sortOps (toksLeft, Star:ops, output)
               (nextOp:rest) -> if (opPres Star) <= (opPres nextOp)
                                then  sortOps ((Star:toksLeft), rest, nextOp:output)
                                else  sortOps (toksLeft, Star:ops, output)
    Slash -> case ops of
               []            ->  sortOps (toksLeft, Slash:ops, output)
               (nextOp:rest) -> if (opPres Slash) <= (opPres nextOp)
                                then  sortOps ((Slash:toksLeft), rest, nextOp:output)
                                else  sortOps (toksLeft, Slash:ops, output)
    LParen -> sortOps (toksLeft, LParen:ops, output)
    RParen -> case ops of
                (LParen:rest) -> sortOps (toksLeft, rest, output)
                (nextOp:rest) -> sortOps (RParen:toksLeft, rest, nextOp:output)
                _             -> error $ show ops
    Stop  -> case ops of
               []            -> ([], [], output)
               (nextOp:rest) ->  sortOps (Stop:[], rest, (nextOp:output))
    _     -> error (show toksLeft)
-}

{-
lowestTerms :: Rational -> Rational
    lowestTerms (Rational n d)
        = Rational (signum d * div n divisor) (div abs (d) divisor) where
            divisor = gcd numerator denominator
-}

{-
-- Always call with an starting toks!

Input  Operators   Output
          +           1
2
*
3
+
4

-- input stack output
stack :: [Token] -> ([Token], [Token], [Token])
stakcs toks =
  let
    (input, operators, output) = toks
  case nextToken tok of
    Plus -> case nextToken (nextToken tok) of

isOp p = case p of
  Plus  -> True
  Slash -> True
  Star  -> True
  _     -> False

nextToken :: [Token] -> Token
nextToken []     = Stop
nextToken (e:es) = e

trailingTokens :: [Token] -> [Token]
trailingTokens [] = [Stop]
trailingTokens (e:es) = es

-- Parse works for right associtiavity 

makeExp :: ArithExp -> [Token] -> ArithExp
makeExp exp1 toks =
  let
    exp2 = parse (tail toks)
  in
    case nextToken toks of
      (Num a) -> Number a 
      Plus    -> Add (exp1) (exp2)
      Star    -> Multi (exp1) (exp2)
      Slash   -> Div (exp1) (exp2)
      Stop    -> error "called on stop"
      _       -> error "not plus or stop in makeExp"

parse :: [Token] -> ArithExp
parse ((Num m):[]) = Number m 
parse toks = 
   makeExp (getNum.head $ toks) (trailingTokens toks)

parser :: [Token] -> ArithExp
parser toks =
  let lhs  = nextToken toks
      toks' = tail toks
  case nextToken toks of
    Plus ->

pop :: [Token] -> [Token]
pop []     = []
pop (e:es) = es

nextToken :: [Token] -> Token
nextToken []     = Stop
nextToken (e:es) = e

makeExpressions :: [Token] -> (ArithExp, [Token])
makeExpressions toks =
  let (exp1, toks') = getNextTerm toks
  in
     case nextToken toks' of
       Plus  -> let (exp2, toks'') = getNextTerm (tail toks') in
                    (Add exp1 exp2, toks'')
       Star  -> let (exp2, toks'') = getNextTerm (tail toks') in
                    (Multi exp1 exp2, toks'')
       Slash -> let (exp2, toks'') = getNextTerm (tail toks') in
                    (Div exp1 exp2, toks'')                    
       Stop  -> (exp1, [])
                                    
getNextTerm :: [Token] -> (ArithExp, [Token])
getNextTerm (tok : []) = 
  case tok of
    (Num m)    -> (Number m, [])
    Stop       -> (Number (-1), [])
getNextTerm toks =
  makeExpressions toks

parse :: [Token] -> ArithExp
parse toks =
  let (exp, toks') = makeExpressions toks in
  if length toks' == 0
  then exp
  else Number (-2)

isPlus :: Token -> Bool
isPlus (Plus) = True
isPlus _      = False

isOp :: Token -> Bool
isOp c = case c of
  Plus  -> True
  Star  -> True
  Slash -> True
  _     -> False


joinExps :: [Token] -> [[Token]]
joinExps []     = []
joinExps (Plus : xs) = [Plus] : joinExps xs
joinExps exps   = exp : joinExps rest where
  (exp, rest)   = span (not.isPlus) exps

reduce :: [[Token]] -> [[Token]]
reduce (e:exps) = (e ++ (head exps)) : (tail exps)

parse :: [Token] -> ArithExp
parse toks' =
  let toks  = joinExps toks'in
    if ()
    then case 

tokenize :: [Char] -> [[Char]]
tokenize []     = []
tokenize (hd:tl) 
  | elem hd "+*/()" = [hd]: tokenize tl
  | isNum hd        = num : tokenize rest
  | otherwise     = tokenize tl
  where
    isNum c = or [isDigit c, c=='-'] 
    (num, rest)  = span isNum (hd:tl)

joinExps :: [[Char]] -> [[[Char]]]
joinExps [[]] = [[[]]]
joinExps list = [exp] : joinExps rest where
  (exp, rest) = span (/="+") list  
-}

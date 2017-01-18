-- Austin Clyde

import Lab3
import Data.List

-- Eval
-- if the base 4 work, then combining them should work
-- since its all int operations
test0 = eval (Number 1) == 1
test1 = eval (Add (Number 1) (Number 1)) == 2
test2 = eval (Multi (Number 1) (Number 0)) == 0
test3 = eval (Div (Number 0) (Number 1)) == 0
test4 = eval (Add (Number 1) (Div (Multi (Number 2) (Number 3)) (Number 4))) == 2


-- Tokenize
test5 = tokenize "" == [Stop]
test6 = tokenize " " == [Stop]
test7 = tokenize "+*/()" == [Plus, Star, Slash, LParen, RParen, Stop]
test8 = tokenize "1 -2 0" == [Num 1, Num (-2), Num 0, Stop]
test9 = tokenize "((1)+(-2))" == [LParen, LParen, Num 1, RParen, Plus, LParen,
                                  Num (-2), RParen, RParen, Stop]


-- Parse
test10 = parse [Num 1, Num 2, Plus, Stop] == Add (Number 1) (Number 2)
test11 = (parse $ tokenize "1*2+8/2") == Add (Multi (Number 1) (Number 2)) (Div (Number 8) (Number 2))
test12 = (parse $ tokenize "(9/(3*(1+2)))") == Div (Number 9) (Multi (Number 3) (Add (Number 1) (Number 2)))
test13 = (eval . parse $ tokenize "(-10+(2+4)*(10+30/5*3+-6))*(3+2)+5") == 615
test14 = (eval . parse $ tokenize "5+(-10+(2+4)*(10+30/5*3+-6))*(3+2)") == 615
test15 = (eval . parse $ tokenize "5*(-10+(2+4)*(10+30/5*3+-6))*(3+2)") == 3050

tests = [test0,
         test1,
         test2,
         test3,
         test4,
         test5,
         test6,
         test7,
         test8,
         test9,
         test10,
         test11,
         test12,
         test13,
         test14,
         test15
        ]
  

testAll = and tests
testPasses = sum $ map fromEnum tests
testFirstFailIndex = elemIndex False tests

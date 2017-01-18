Austin Clyde
Lecture 2

—2.1*
product [] = 1
product (x:xs) = x * product(xs)

--product of squares from 1 to 10
ans = product . map (^2) $ [1..10]

--test
--product [] = 1
--product [-1, -2] = 2
--product [1.5, 2] = 3.0 

--2.2*
divisibleBy d n = mod n d == 0

allp [] x = True
allp (p:ps) x = p x && allp ps

filterAll = filter .  allp

--checks: result == result2 == result3 = True

result = sum
         . take 100
         . filter (divisibleBy 2)
         . filter (divisibleBy 3)
         . filter (not . divisibleBy 4)
         . filter (not . divisibleBy 9)
         $ [0..]

result2 = sum
          .take 100
          . filter (allp [ divisibleBy 2
                         , divisibleBy 3
                         , not . divisibleBy 4
                         , not . divisibleBy 9
                         ])
          $ [0..]

result3 = sum
          . take 100
          . filterAll [ divisibleBy 2
                         , divisibleBy 3
                         , not . divisibleBy 4
                         , not . divisibleBy 9
                         ]
          $ [0..]
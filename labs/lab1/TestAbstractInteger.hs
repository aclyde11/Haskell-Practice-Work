-- Just a scratch file with some examples of some lines you
-- can copy-paste to ghci to test.
--
-- You do not need to turn this in.

add two three == five
add negativeTwo three == one
add two negativeThree == negativeOne
dd negativeTwo negativeThree == negativeFive

divide ten three == three
divide ten negativeThree == negativeThree
divide negativeTen three == negativeFour
divide negativeTen negativeThree == four

modulo ten three == one
modulo ten negativeThree == one
modulo negativeTen three == two
modulo negativeTen negativeThree == two

solveRPN ["3", "4", "-", "2", "+", "10", "*"] == toAbstract 10

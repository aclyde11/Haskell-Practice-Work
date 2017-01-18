-- Austin Clyde Lab 2

import HLab2
import Data.List


test0 = intersects (Line (Point 1 1 0)) (LineSegment (Point 0 0 1) (Point 1 1 1)) == True
test1 = intersects (Line (Point (-1) 1 (-2))) (LineSegment (Point 0 0 1) (Point 1 1 1)) == False
test2 = intersects (Line (Point 1 1 (-1))) (LineSegment (Point 0 0 1) (Point 1 1 1)) == True
test3 = intersects (Line (Point (-1) 1 0)) (LineSegment (Point (-1) 1 1) (Point (-1) 3 1)) == False 


{-
austin@Austin's_Macbook:~/cmsc161/classes$ ./3l2
Hello. I am a HAL 9000 series computer.
Good morning, Austin.
test4 = intersectsBB (Point (-1) 1 0) (BoundBox (Point 1 1 1) (Point 1 1 2)) == True
test5 = intersectsBB (Point (-1) 1 0) (BoundBox (Point 1 1 1) (Point 1 1 (-2))) == True
test6 = intersectsBB (Point (-1) 1 0) (BoundBox (Point (-1) 1 1) (Point 0 3 1)) == False
-}



tests = [test0,
         test1,
         test2,
         test3
        ]

testAll = and tests
testPasses = sum $ map fromEnum tests
testFirstFailIndex = elemIndex False tests

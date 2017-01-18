-- Austin Clyde Lab 2

import Lab2
import Data.List

-- intersects with LineSegment and VerticalLine
test0  = intersects (LineSegment (Point 0 0) (Point 1 1)) (VerticalLine 0.5) == True
test1  = intersects (LineSegment (Point 1 1) (Point 0 0)) (VerticalLine 0.5) == True
test2  = intersects (LineSegment (Point 0 0) (Point 1 1)) (VerticalLine (-0.5)) == False
test3  = intersects (LineSegment (Point 1 1) (Point 1 1)) (VerticalLine 1) == True
test4  = intersects (LineSegment (Point (-1) (-1)) (Point 1 1)) (VerticalLine 0) == True

-- intersects with LineSegments and Lines
test5  = intersects (LineSegment (Point 0 0) (Point 1 1)) (Line (-1) 0) == True
test6  = intersects (LineSegment (Point 0 0) (Point 1 1)) (Line (1) 0) == True
test7  = intersects (LineSegment (Point 0 0) (Point 1 1)) (Line (-1) 3) == False
test8  = intersects (LineSegment (Point 2 0) (Point (-1) 3)) (Line (1) 0) == True

-- boundShape with Circle
test9  = boundShape (Circle (Point 1 1) 1) == BoundingBox (Point 0 0) (Point 2 2)
test10 = boundShape (Circle (Point 3 4) 3) == BoundingBox (Point 0 1) (Point 6 7)

-- boundShape with Triangle
test11 = boundShape (Triangle (Point 0 0) (Point 0 1) (Point 1 0)) == BoundingBox (Point 0 0) (Point 1 1) 
test12 = boundShape (Triangle (Point 0 1) (Point 0 0) (Point 1 0)) == BoundingBox (Point 0 0) (Point 1 1) 
test13 = boundShape (Triangle (Point 0 1) (Point 1 0) (Point 0 0)) == BoundingBox (Point 0 0) (Point 1 1) 
test14 = boundShape (Triangle (Point (-1) 1) (Point 1 0) (Point 0 10)) == BoundingBox (Point (-1) 0) (Point 1 10)

-- boundShape with Quad
test15 = boundShape (Quad (Point (-1) (-1)) (Point 1 1) (Point (-1) 1) (Point 1 (-1))) ==
                    BoundingBox (Point (-1) (-1)) (Point 1 1)


-- intersect with linesegment and Parabola
test16 = intersects (LineSegment (Point 0 0) (Point 3 4)) (Parabola 1 0 0) == True
test17 = intersects (LineSegment (Point (-1) 0) (Point (-1) (-2))) (Parabola 1 0 0) == False
test18 = intersects (LineSegment (Point 0 0) (Point 3 4)) (Parabola (-1) 0 0) == True
test19 = intersects (LineSegment (Point 0 0) (Point 3 4)) (Parabola (-1) 0 (-1)) == False


-- HPoint and HLine
test20 = intersects (LineSegment (HPoint 0 0 1) (HPoint 1 1 1)) (HLine 1 1 0) == True
test21 = intersects (LineSegment (HPoint 0 0 1) (HPoint 1 1 1)) (HLine (-1) 1 (-2))== False
test22 = intersects (LineSegment (HPoint 0 0 1) (HPoint 1 1 1)) (HLine 1 1 (-1)) == True
test23 = intersects (LineSegment (HPoint (-1) 1 1) (HPoint (-1) 3 1)) (HLine (-1) 1 0)== False 




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
         test15,
         test16,
         test17,
         test18,
         test19,
         test20,
         test21,
         test22,
         test23
        ]
testAll = and tests
testPasses = sum $ map fromEnum tests
testFirstFailIndex = elemIndex False tests

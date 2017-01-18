module HLab2 (
  Point(Point),
  LineSegment(LineSegment),
  Path(Line),
  BoundBox(BoundBox),
  intersects,
  cross,
  isBetween,
  slope,
  intercept,
  --intersectsBB
) where

-- In a homogenous system, points are projections (so lines) and lines are projective planes.
-- Hence, a line can be a point (projection) or the cartesian notion of a line. Therefore,
-- they share the same coordinate system (a, b, c) can represent the cooridate (a/c, b/c)
-- but could also be the line ax + by +xz = 0. Hence the designation Point or Line is rather
-- a trival distinction

data Point = Point Double Double Double
    deriving (Show)

data LineSegment = LineSegment Point Point
    deriving (Show)

data Path =
      -- Lines y = mx + b => 0 = ax + by + cz = 0,
      -- So here I take m and b and transform it
      -- to homogenous for calculations
      Line Double Double
      -- So the lab doesn't ask for this in
      -- Projective coordinates...
      -- And it turns out this is really only
      -- Possible with matrix decomposition
      -- (Maxwell 1946)
    | Parabola Double Double Double 
    deriving (Show)

data Shape =
    Circle Point Double
  | Triangle Point Point Point
  | Quad Point Point Point

data BoundBox = BoundBox Point Point

{-
intersectsBB :: Point -> BoundBox -> Bool
intersectsBB p (BoundBox (Point d1 d2 d3) (Point b1 b2 b3)) =
  let
    a = Point (d1/d3) (b2/b3) 1
    c = Point (b1/b3) (d1/d3) 1
    d = Point d1 d2 d3
    b = Point b1 b2 b3
    segments = [LineSegment a b,
                LineSegment b c,
                LineSegment c d,
                LineSegment d a
               ]
  in or $ map (intersects p) segments
-}

cross :: Point -> Point -> Point
cross (Point a1 b1 c1) (Point a2 b2 c2) =
  Point  (b1*c2 - b2*c1) (a2 *c1 - a1 * c2) (a1*b2 - a2*b1)

isBetween :: Point -> Point -> Point -> Bool
isBetween (Point a1 b1 c1) (Point a2 b2 c2) (Point a3 b3 c3) =
  and [(a1/c1) <= (a3/c3),
       (a3/c3) <= (a2/c2),
       (b1/c1) <= (b3/c3),
       (b3/c3) <= (b2/c2)
      ]
  
intersects :: Path -> LineSegment -> Bool
intersects (Line m y0) (LineSegment a b) =
  let p  = cross (cross a b) (Point m (-1) y0)
  in isBetween a b p
intersects (Parabola a d f) (LineSegment ) =
  let
    p = (p


  
  
  
slope (Point a b c) = (-a)/b
intercept (Point a b c) = (-c)/b

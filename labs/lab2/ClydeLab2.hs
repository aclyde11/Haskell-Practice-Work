-- Austin Clyde
-- Lab 2

module Lab2(
   Point(Point, HPoint),
   LineSegment(LineSegment),
   Path(Line, VerticalLine, Parabola, HLine),
   Shape(Circle, Triangle, Quad),
   BoundingBox(BoundingBox),
   intersects,
   intersectionPoints,
   boundShape
)  where

data Point =
    Point Double Double
  | HPoint Double Double Double 
  deriving (Show, Eq)

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point
   deriving (Show, Eq)


-- A Path is a 2D path in the xy-plane.  The idea is that Path can be
-- extended to support straight lines, curves, and arbitrary paths,
-- but currently there is only one data constructor for Path: Line.
data Path =
  -- given in y=mx+b
    Line Double Double
  | HLine Double Double Double
  -- given in x=y
  | VerticalLine Double
  -- given in standard form y=ax^2 + bx + c
  | Parabola Double Double Double
  deriving (Show, Eq)

data Shape =
    Circle Point Double
  | Triangle Point Point Point
  | Quad Point Point Point Point 
    deriving (Show, Eq)

-- BoundingBox represents an axis-aligned rectangle
-- represented by bottom left corner and upper right corner
data BoundingBox = BoundingBox Point Point
  deriving (Show, Eq)

intersects :: LineSegment -> Path -> Bool
intersects (LineSegment (Point x1 y1) (Point x2 y2)) p =
  case p of
    VerticalLine x -> min x1 x2 <= x && x <= max x1 x2 
    Line a c -> let b = (y2 - y1) / (x2 - x1)
                    d = y2 - b*x2
                    p = (a*d - b*c) / (a - b)
                in if (a == b)
                     then c == d
                     else min y1 y2 <= p && p <= max y1 y2 
    Parabola a b c -> if a == 0
                      then intersects (LineSegment (Point x1 y1) (Point x2 y2)) (Line b c)
                      else
                        let ps = intersectionPoints (Line ((y2-y1)/(x2-x1))
                                                    (y2-x2*(y2-y1)/(x2-x1))) p
                        in 0 < (length $ filter
                                        (isBetween (Point x1 y1) (Point x2 y2))
                                        ps)
    HLine _ _ _        -> intersects (LineSegment (HPoint x1 y1 1) (HPoint x2 y2 1)) p
intersects (LineSegment a b) (HLine x y z)  =
  let p  = cross (cross a b) (HPoint x y z)
  in isBetween a b p
intersects (LineSegment (HPoint a b c) (HPoint x y z)) p =
  intersects (LineSegment (Point (a/c) (b/c)) (Point (x/z) (y/z))) p

boundShape (Circle (Point cx cy) r) =
  BoundingBox (Point (cx - r) (cy - r)) (Point (cx + r) (cy + r))
boundShape (Triangle (Point ax ay) (Point bx by) (Point cx cy))           =
  BoundingBox (Point (minimum [ax, bx, cx]) (minimum [ay, by, cy]))
              (Point (maximum [ax, bx, cx]) (maximum [ay, by, cy]))
boundShape (Quad (Point ax ay) (Point bx by) (Point cx cy) (Point dx dy)) =
  BoundingBox (Point (minimum [ax, bx, cx, dy]) (minimum [ay, by, cy, dy]))
              (Point (maximum [ax, bx, cx, dy]) (maximum [ay, by, cy, dy]))

intersectsBB (BoundingBox (Point ax ay) (Point cx cy)) l =
  let
    a = Point ax cy
    b = Point cx cy
    c = Point cx ay
    d = Point ax ay
    edges = [LineSegment a b, LineSegment b c, LineSegment c d, LineSegment d a]
  in or $ map (flip intersects l) edges

mightIntersectShape = intersectsBB . boundShape

-- HPoint and HLine helpers

cross (HPoint a1 b1 c1) (HPoint a2 b2 c2) =
  HPoint  (b1*c2 - b2*c1) (a2 *c1 - a1 * c2) (a1*b2 - a2*b1)

isBetween (Point a1 b1) (Point a2 b2) (Point a3 b3) =
  and [min a2 a1 <= a3,
       a3 <= max a1 a2,
       min a1 b1 <= b3,
       b3 <= max b1 b2
      ]
isBetween (HPoint a1 b1 c1) (HPoint a2 b2 c2) (HPoint a3 b3 c3) =
  and [min (a2/c2) (a1/c1) <= (a3/c3),
       (a3/c3) <= max (a1/c1) (a2/c2),
       min (b2/c2) (b1/c1) <= (b3/c3),
       (b3/c3) <= max (b1/c1) (b2/c2)
      ]

intersectionPoints :: Path -> Path -> [Point]
intersectionPoints (Line m n) (Parabola a b' c') =
  let
    b    = b' - m
    c    = c' - n
    conj = b^2 - 4 * a * c
  in
    if conj < 0
    then []
    else if conj == 0
    then let x=(-b)/(2*a) in [Point x (m*x + n)]
    else let x_1 = ((-b) + conj)/(2*a)
             x_2 = ((-b) - conj)/(2*a)
         in [Point x_1 (x_1*m + n), Point x_2 (x_2*m + n)]
      

-- Austin Clyde
-- Week 1, Lecture 1
-- 1.2, 1.3

-- 1.2*

parabola (x_0, y_0) (v_x, v_y) t =
  let a = 9.80665 in
    (x_0 + v_x * t,
     y_0 + v_y * t - 0.5 * a * (t ^ 2)
    )

-- a few test cases:
-- parabola (0,0) (0,) 0 -> (0.0,0.0)
-- parabola ((-100),(-100)) ((-100),(-100)) (-100) -> (9900.0,-39133.25)
-- parabola (0,0) (10,0) 1 -> (10.0,-4.903325)

-- 1.3*
law_of_cosines a b gamma =
  let deg = gamma * pi / 180 in
    (a ^ 2 + b ^ 2 - 2 * a * b * cos deg) ** 0.5

— a fest test cases:
-- law_of_cosines 3 4 90 -> 5.0
-- law_of_cosines 0 0 0 -> 0.0
-- law_of_cosines (-3) (-4) (-90) -> 5.0

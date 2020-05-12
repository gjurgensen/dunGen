module Misc where

import Data.Matrix

type Coord = (Int,Int)

liftPair  f (x0,y0) = (f x0, f y0)
liftPair2 f (x0,y0) (x1,y1) = (f x0 x1, f y0 y1)
liftPair3 f (x0,y0) (x1,y1) (x2,y2) = (f x0 x1 x2, f y0 y1 y2)

-- *inclusive* range check
x `inRange` (min, max) = x >= min && x <= max

(x,y) `inRect` ((minX,minY),(maxX,maxY)) = x `inRange` (minX, maxX)
                                        && y `inRange` (minY, maxY)

restrict min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x

restrictRect (minX,minY) (maxX,maxY) (x,y) = (restrict minX maxX x,
                                              restrict minY maxY y)

mx `orElse` y = maybe y id mx

-- Conversions of matrix coordinates (begin at (1,1) and are (row,col))
-- to and from typical coordinates (begin at (0,0), and are (x,y))
fromMatCoord (i, j) = (j - 1, i - 1)
toMatCoord   (x, y) = (y + 1, x + 1)

adjGetElem m x y = uncurry getElem (toMatCoord (x,y)) m
adjSetElem m x y elem = setElem elem (toMatCoord (x,y)) m

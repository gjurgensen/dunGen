module Misc where

import Data.Matrix

-- *inclusive* range check
x `inRange` (min, max) = x >= min && x <= max

(x,y) `inRect` ((minX,minY),(maxX,maxY)) = x `inRange` (minX, maxX)
                                        && y `inRange` (minY, maxY)

mx `orElse` y = maybe y id mx

-- Conversions of matrix coordinates (begin at (1,1) and are (row,col))
-- to and from typical coordinates (begin at (0,0), and are (x,y))
fromMatCoord (i, j) = (j - 1, i - 1)
toMatCoord   (x, y) = (y + 1, x + 1)

adjGetElem m x y = uncurry getElem (toMatCoord (x,y)) m

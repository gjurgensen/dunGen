module Misc where

-- *inclusive* range check
x `inRange` (min, max) = x >= min && x <= max

(x,y) `inRect` ((minX,minY),(maxX,maxY)) = x `inRange` (minX, maxX)
                                        && y `inRange` (minY, maxY)

mx `orElse` y = maybe y id mx

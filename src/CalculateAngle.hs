module CalculateAngle (calculateAngle) where

import Graphics.UI.GLUT

calculateAngle :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat
calculateAngle x1 y1 x2 y2 | x1 == x2 && y1 == y2 = 0
                           | x1 == x2 && y1 < y2 = 3*pi/2
                           | x1 == x2 && y1 > y2 = pi/2
                           | y1 == y2 && x1 > x2 = 0
                           | y1 == y2 && x1 < x2 = pi
                           | y1 > y2 && x1 > x2 = atan((y1 - y2)/(x1 - x2))
                           | y1 > y2 && x1 < x2 = pi/2 + atan((x2 - x1)/(y1 - y2))
                           | y1 < y2 && x1 < x2 = pi + atan((y2 - y1)/(x2 - x1))
                           | y1 < y2 && x1 > x2 = 3*pi/2 + atan((x1 - x2)/(y2 - y1))
                           | otherwise = 0           -- should not happen
{-
       x2,y2
       /
      /
     /\ <-result
    /__)____
  x1,y1

-}

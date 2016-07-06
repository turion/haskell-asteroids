module CalculateAngle (calculateAngle) where

calculateAngle :: Int -> Int -> Int -> Int -> Float
calculateAngle x1 y1 x2 y2 | x1 == x2 && y1 == y2 = 0
                       | x1 == x2 && y1 < y2 = 3*pi/2
                       | x1 == x2 && y1 > y2 = pi/2
                       | y1 == y2 && x1 > x2 = 0
                       | y1 == y2 && x1 < x2 = pi
                       | y1 > y2 && x1 > x2 = atan((fromIntegral y1 - fromIntegral y2)/(fromIntegral x1 - fromIntegral x2))
                       | y1 > y2 && x1 < x2 = pi/2 + atan((fromIntegral x2 - fromIntegral x1)/(fromIntegral y1 - fromIntegral y2))
                       | y1 < y2 && x1 < x2 = pi + atan((fromIntegral y2 - fromIntegral y1)/(fromIntegral x2 - fromIntegral x1))
                       | y1 < y2 && x1 > x2 = 3*pi/2 + atan((fromIntegral x1 - fromIntegral x2)/(fromIntegral y2 - fromIntegral y1))
                       | otherwise = 10           -- should not happen
{-
       x2,y2
       /
      /
     /\ <-result
    /__)____
  x1,y1

-}

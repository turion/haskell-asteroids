module AI (rotateClockwiseToAim) where

import CalculateAngle

-- this function gives true if the enemy should turn in clockwise direction, false otherwise
-- takes coordinates of the ship, coordinates of an enemy and the angle to what the enemy is facing
rotateClockwiseToAim :: Int -> Int -> Int -> Int -> Float -> Bool
rotateClockwiseToAim x1 y1 x2 y2 angle | x1 == x2 && y1 == y2 = True
                      | phi < pi && angle < phi = False
                      | phi < pi && angle > pi + phi = False
                      | phi > pi && angle > (phi - pi) && angle < phi = False
                      | otherwise = True
    where
      phi = calculateAngle x1 y1 x2 y2

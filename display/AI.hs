module AI (rotateClockwiseToAim) where

import CalculateAngle
import Datatypes

-- this function gives true if the enemy should turn in clockwise direction, false otherwise
-- takes coordinates of the ship, coordinates of an enemy and the angle to what the enemy is facing
rotateClockwiseToAim :: Int -> Int -> Int -> Int -> Float -> Bool
rotateClockwiseToAim x1 y1 x2 y2 angle | x1 == x2 && y1 == y2 = True
                      | phi < pi && angle/180*pi < phi = False
                      | phi < pi && angle/180*pi > pi + phi = False
                      | phi > pi && angle/180*pi > (phi - pi) && angle/180*pi < phi = False
                      | otherwise = True
    where
      phi = calculateAngle x1 y1 x2 y2
--doObjectsMeet (GameObject (Vector 0.5 0.5) (Vector 0.2 0.1) 135.0 1.0 EnemyShip) (GameObject (Vector (-0.8) 0.4) (Vector 0.1 0.1) 160.0 0.5 Asteroid)

doObjectsMeet :: GameObject -> GameObject -> Bool
doObjectsMeet (GameObject {location = Vector x1 y1, velocity = Vector vx1 vy1, orientation = o1, scaleObject = s1, gameObjectType = objType1}) (GameObject {location = Vector x2 y2, velocity = Vector vx2 vy2, orientation = o2, scaleObject = s2, gameObjectType = objType2})
    | y2 > y1 && vy2 > 0 && vy1 < 0 = False
    | y1 > y2 && vy1 > 0 && vy2 < 0 = False
    | x2 > x1 && vx2 > 0 && vx1 < 0 = False
    | x1 > x2 && vx1 > 0 && vx2 < 0 = False
    | dx * dvy == dy * dvx = True
    | otherwise = False
    where
      dx = x2 - x1
      dy = y2 - y1
      dvx = vx2 - vx1
      dvy = vy2 - vy1

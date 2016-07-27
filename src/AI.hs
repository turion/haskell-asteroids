module AI (
    rotateClockwiseToAim,
    aim,
    rotateAsteroid
  ) where

import CalculateAngle
import Datatypes
import Physics
import Data.Fixed
import UI
import FRP.Yampa
import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace

-- this function gives true if the enemy should turn in clockwise direction, false otherwise
-- takes coordinates of the ship, coordinates of an enemy and the angle to which the enemy is facing
rotateClockwiseToAim :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Bool
rotateClockwiseToAim x1 y1 x2 y2 angle | x1 == x2 && y1 == y2 = True
                      | phi < pi && angle < phi = False
                      | phi < pi && angle > pi + phi = False
                      | phi > pi && angle > (phi - pi) && angle < phi = False
                      | otherwise = True
    where
      phi = calculateAngle x1 y1 x2 y2

doObjectsMeet :: GameObject -> GameObject -> Bool
doObjectsMeet (GameObject {location = Vector x1 y1, velocity = Vector vx1 vy1, orientation = o1, gameObjectType = objType1}) (GameObject {location = Vector x2 y2, velocity = Vector vx2 vy2, orientation = o2, gameObjectType = objType2})
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

aim :: ID -> GameLevel -> UserInput
aim enemyShipId level | rotateClockwiseToAim x1 y1 x2 y2 phi == True = UserInput 0.0 (-0.7)
                      | otherwise = UserInput 0.0 0.7
    where
      x1 = x (location ship)
      y1 = y (location ship)
      x2 = x (location enemyship)
      y2 = y (location enemyship)
      o = (orientation enemyship)
      ( ship:_) = [ object | object <- objects level, gameObjectType object == Ship  ]
      ( enemyship:_) = [ object | object <- objects level, objectId object == enemyShipId  ]
      --phi = mod2Pi $ o + pi / 2
      phi = mod' (o + pi / 2) (pi * 2)

rotateAsteroid :: GameObjectType -> UserInput
rotateAsteroid (Asteroid s sh r) = UserInput 0.0 r
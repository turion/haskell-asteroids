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

doObjectsCollide :: GameObject -> GameObject -> Bool
doObjectsCollide object1 object2 = t < 0 && radius (gameObjectType object1) + radius (gameObjectType object2) <= d where
  t = closest (location object1) (velocity object1) (location object2) (velocity object2)
  d = norm $ ((location object1) ^-^ (location object2)) ^+^ t *^ ((velocity object1) ^-^ (velocity object2))

closest :: Location -> Velocity -> Location -> Velocity -> GLfloat
closest l1 v1 l2 v2 =  ((v1 ^-^ v2) `dot` (l1 ^-^ l2)) / ((v1 ^-^ v2) `dot` (v1 ^-^ v2))

aim :: ID -> GameLevel -> UserInput
aim enemyShipId level | speedTooFast enemyShip && facingSpeedDirection enemyShip == False = fastTurnInTheSpeedDirection enemyShip
                      | speedTooFast enemyShip && facingSpeedDirection enemyShip == True = UserInput (0.3) 0 False
                      | length (approachingObject enemyShip level) > 0 = tryToAvoid enemyShip $ head $ approachingObject enemyShip level
                      | rotateClockwiseToAim x1 y1 x2 y2 (orienationAngle enemyShip) == True = UserInput approachingSpeed (-1) False
                      | otherwise = UserInput approachingSpeed 1 False
    where
      x1 = x (location ship)
      y1 = y (location ship)
      x2 = x (location enemyShip)
      y2 = y (location enemyShip)
      ( ship:_) = [ object | object <- objects level, (gameObjectType object) == Ship ]
      ( enemyShip:_) = [ object | object <- objects level, objectId object == enemyShipId  ]
      approachingSpeed = 0              -- make it higher (about 0.05) to make the asteroids try to ram the ship

approachingObject :: GameObject -> GameLevel -> [GameObject]
approachingObject enemyShip level | length closeObjects > 0 = [closestFromList enemyShip closeObjects]
                                  | otherwise = []
  where
    closeObjects = [ object | object <- objects level, enemyShip /= object && gameObjectType object /= Ship  && distance enemyShip object < d && doObjectsCollide enemyShip object == True]
    d = 0.4                -- range at which the enemy ships start to avoid the asteroids or other enemy ships

closestFromList :: GameObject -> [GameObject] -> GameObject
closestFromList enemyShip closeObjects = head [ object | object <- closeObjects, distance enemyShip object == min]
  where
    min = minimum [ distance enemyShip obj | obj <- closeObjects]

distance :: GameObject -> GameObject -> GLfloat
distance o1 o2 = norm ((location o1) ^-^ (location o2))

rotateAsteroid :: GameObjectType -> UserInput
rotateAsteroid (Asteroid s sh r) = UserInput 0.0 r False

tryToAvoid :: GameObject -> GameObject -> UserInput
tryToAvoid obj1 obj2 | facingAvoidingDirection obj1 obj2 == False = fastTurnInTheAvoidingDirection obj1 obj2
                     -- | facingAvoidingDirection obj1 obj2 == True = UserInput (0.6) 0
                     | otherwise = UserInput 1 0 False

speedTooFast :: GameObject -> Bool
speedTooFast o = norm (velocity o) > 0.1

fastTurnInTheSpeedDirection :: GameObject -> UserInput
fastTurnInTheSpeedDirection obj | rotateClockwiseToAim 0 0 (x (velocity obj)) (y (velocity obj)) (orienationAngle obj) == True = UserInput 0 (-3) False
                                | otherwise = UserInput 0 3 False

fastTurnInTheAvoidingDirection :: GameObject -> GameObject -> UserInput
fastTurnInTheAvoidingDirection obj1 obj2 | rotateClockwiseToAim (x (location obj2)) (y (location obj2)) (x (location obj1)) (y (location obj1)) (orienationAngle obj1) == True = UserInput 0 (5) False
                                         | otherwise = UserInput 0 (-5) False

facingSpeedDirection :: GameObject -> Bool
facingSpeedDirection obj = (velocityAngle + angleRange) > o && (velocityAngle - angleRange) < o
  where
    o = orienationAngle obj
    angleRange = 0.1
    velocityAngle = calculateAngle 0 0 (x (velocity obj)) (y (velocity obj))

orienationAngle :: GameObject -> GLfloat
orienationAngle o = mod' ((orientation o) + pi / 2) (pi * 2)

facingAvoidingDirection :: GameObject -> GameObject -> Bool
facingAvoidingDirection obj1 obj2 = (avoidingAngle + angleRange) > o && (avoidingAngle - angleRange) < o
  where
    o = orienationAngle obj1
    angleRange = 0.3
    avoidingAngle = calculateAngle (x (location obj1)) (y (location obj1)) (x (location obj2)) (y (location obj2))
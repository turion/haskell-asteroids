module Generator (
    generateLevel
  ) where

import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import Control.Monad
import System.Random

import Datatypes
import Physics

generateLevel :: Int -> Int -> StdGen -> GameLevel
generateLevel enemyAmount asteroidAmount gen = GameLevel $ generateSeveralObjects enemyAmount asteroidAmount gen

generateSeveralObjects :: Int -> Int -> StdGen -> [GameObject]
generateSeveralObjects 0 0 _ = [GameObject (Vector 0 0) (Vector 0 0) 0 0 Ship ]
generateSeveralObjects 0 asteroids g = first:rest
  where
    rest = generateSeveralObjects 0 (asteroids - 1) g2
    (first, g2) = generateGameObject (Asteroid 1.0 (Shape []) 0) rest g
generateSeveralObjects enemies asteroids g = first:rest
  where
    rest = generateSeveralObjects (enemies-1) asteroids g2
    (first, g2) = generateGameObject EnemyShip rest g

generateGameObject :: GameObjectType -> [GameObject] -> StdGen -> (GameObject, StdGen)
generateGameObject objType objects g = (GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) velocity (o*2*pi) (length objects) newObjectType, gLast)
  where
    (x, g2) = rand g
    (y, g3) = rand g2
    (vx, g4) = rand g3
    (vy, g5) = rand g4
    (o, g6) = rand g5
    (r, g7) = rand g6
    (randomRotation, g8) = rand g7
    (randomShape, gLast) = generateAsteroidShape g8
    randomRadius | r <= 0.33 = 1.0
                 | r <= 0.66 = 1.5
                 | otherwise = 2.0
    newObjectType | objType == EnemyShip = objType
                  | otherwise = Asteroid randomRadius randomShape (2 * randomRotation - randomRotation)
    velocityRange = 0.03
    rotationRange = 0.8
    velocity = Vector (vx * 2 * velocityRange - velocityRange) (vy * 2 * velocityRange - velocityRange)

generateAsteroidShape :: StdGen -> (Shape, StdGen)
generateAsteroidShape g = (Shape [p1, p2, p3, p4, p5, p6, p7, p8], gLast)
  where
    a = 0.013
    b = 0.01
    (r1, g1) = randomR (2*a, 3*a) g
    (r2, g2) = randomR (2*b, 3*b) g1
    (r3, g3) = randomR (2*a, 3*a) g2
    (r4, g4) = randomR (2*b, 3*b) g3
    (r5, g5) = randomR (2*a, 3*a) g4
    (r6, g6) = randomR (2*b, 3*b) g5
    (r7, g7) = randomR (2*a, 3*a) g6
    (r8, gLast) = randomR (2*b, 3*b) g7
    p1 = Vector 0 r1
    p2 = Vector r2 r2
    p3 = Vector r3 0
    p4 = Vector r4 (-r4)
    p5 = Vector 0 (-r5)
    p6 = Vector (-r6) (-r6)
    p7 = Vector (-r7) 0
    p8 = Vector (-r8) r8

rand :: StdGen -> (GLfloat , StdGen )
rand g = random g :: (GLfloat , StdGen )



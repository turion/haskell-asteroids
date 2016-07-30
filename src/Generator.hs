module Generator (
    generateLevel
  ) where

import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import Control.Monad
import System.Random
--import Control.Monad.Random.Class

import Datatypes
import Physics

generateLevel :: Int -> Int -> IO GameLevel
generateLevel enemyAmount asteroidAmount = do
  objects <- generateSeveralObjects enemyAmount asteroidAmount
  return $ GameLevel objects

generateSeveralObjects :: Int -> Int -> IO [GameObject]
generateSeveralObjects 0 0 = do return [GameObject (Vector 0 0) (Vector 0 0) 0 0 Ship ]
generateSeveralObjects 0 asteroids = do
  rest <- generateSeveralObjects 0 (asteroids - 1)
  first <- generateGameObject (Asteroid 1.0 (Shape []) 0) rest
  return $ first:rest
generateSeveralObjects enemies asteroids = do
  rest <- generateSeveralObjects (enemies-1) asteroids
  first <- generateGameObject EnemyShip rest
  return $ first:rest

generateGameObject :: GameObjectType -> [GameObject] -> IO GameObject
generateGameObject objType objects = do
  randomRadius <- ([1.0, 1.5, 2.0] !!) <$> randomRIO(0,2)
  let velocityRange = 0.03
      rotationRange = 0.8
  x <- randomIO
  y <- randomIO
  o <- randomIO
  randomRotation <- randomRIO (-rotationRange, rotationRange)
  v1 <- randomRIO (-velocityRange, velocityRange)
  v2 <- randomRIO (-velocityRange, velocityRange)
  randomShape <- generateAsteroidShape
  let newObjectType | objType == EnemyShip = objType
                    | otherwise = Asteroid randomRadius randomShape randomRotation
  let newObject = GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector v1 v2) (o*2*pi) (length objects) newObjectType
  if not (overlapAny newObject objects)
    then return newObject
    else generateGameObject newObjectType objects

generateAsteroidShape :: IO Shape
generateAsteroidShape = do
  let
    a = 0.013
    b = 0.01
  r1 <- randomRIO (2*a, 3*a)
  r2 <- randomRIO (2*b, 3*b)
  r3 <- randomRIO (2*a, 3*a)
  r4 <- randomRIO (2*b, 3*b)
  r5 <- randomRIO (2*a, 3*a)
  r6 <- randomRIO (2*b, 3*b)
  r7 <- randomRIO (2*a, 3*a)
  r8 <- randomRIO (2*b, 3*b)
  let
    p1 = Vector 0 r1
    p2 = Vector r2 r2
    p3 = Vector r3 0
    p4 = Vector r4 (-r4)
    p5 = Vector 0 (-r5)
    p6 = Vector (-r6) (-r6)
    p7 = Vector (-r7) 0
    p8 = Vector (-r8) r8
  return $ Shape [p1, p2, p3, p4, p5, p6, p7, p8]

{-die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)-}

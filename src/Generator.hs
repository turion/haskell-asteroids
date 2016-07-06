module Generator (
    generateLevel
  ) where

import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import Control.Monad
import Datatypes
import System.Random

generateLevel :: Int -> Int -> IO GameLevel
generateLevel enemyAmount asteroidAmount = do
  objects <- generateSeveralObjects enemyAmount asteroidAmount
  return $ GameLevel objects

generateSeveralObjects :: Int -> Int -> IO [GameObject]
generateSeveralObjects 0 0 = do return [GameObject (Vector 0 0) (Vector 0 0) 0 Ship]
generateSeveralObjects 0 asteroids = do
  rest <- generateSeveralObjects 0 (asteroids - 1)
  first <- generateGameObject (Asteroid 1.0 (Shape [])) rest
  return $ first:rest
generateSeveralObjects enemies asteroids = do
  rest <- generateSeveralObjects (enemies-1) asteroids
  first <- generateGameObject EnemyShip rest
  return $ first:rest

generateGameObject :: GameObjectType -> [GameObject] -> IO GameObject
generateGameObject objType objects = do
  randomRadius <- ([1.0, 1.5, 2.0] !!) <$> randomRIO(0,2)
  x <- randomIO
  y <- randomIO
  o <- randomIO
  randomShape <- generateAsteroidShape
  let newObjectType | objType == EnemyShip = objType
                    | otherwise = Asteroid randomRadius randomShape
  let newObject = GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) newObjectType
  if not (checkIfObjectOverlapsWithOtherObjects newObject objects)
    then return newObject
    else do
       result <- generateGameObject newObjectType objects
       return result

checkIfObjectOverlapsWithOtherObjects :: GameObject -> [GameObject] -> Bool
checkIfObjectOverlapsWithOtherObjects o1 [] = False
checkIfObjectOverlapsWithOtherObjects o1 (o2:os)
  | d < (r1 + r2) = True
  | (length os) > 0 = checkIfObjectOverlapsWithOtherObjects o1 os
  | otherwise = False
  where
    r1 = radius $ gameObjectType o1
    r2 = radius $ gameObjectType o2
    d = norm $ location o1 ^-^ location o2

generateAsteroidShape :: IO Shape
generateAsteroidShape = do
  y1 <- randomIO
  x2 <- randomIO
  x3 <- randomIO
  x4 <- randomIO
  y5 <- randomIO
  x6 <- randomIO
  x7 <- randomIO
  x8 <- randomIO
  let
    p1 = Vector 0                  (y1 * a + a)
    p2 = Vector (x2 * b + b)       (x2 * b + b)
    p3 = Vector (x3 * a + a)                  0
    p4 = Vector (x4 * b + b)    (-(x4 * b + b))
    p5 = Vector 0               (-(y5 * a + a))
    p6 = Vector (-(x6 * b + b)) (-(x6 * b + b))
    p7 = Vector (-(x7 * a + a))               0
    p8 = Vector (-(x8 * b + b))    (x8 * b + b)
    a = 0.025
    b = 0.0175
  return $ Shape [p1, p2, p3, p4, p5, p6, p7, p8]

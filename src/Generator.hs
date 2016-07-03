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
  first <- generateGameObject (Asteroid 1.0) rest
  return $ first:rest
generateSeveralObjects enemies asteroids = do
  rest <- generateSeveralObjects (enemies-1) asteroids
  first <- generateGameObject EnemyShip rest
  return $ first:rest

generateGameObject :: GameObjectType -> [GameObject] -> IO GameObject
generateGameObject (Asteroid s) objects = do
  randomRadius <- ([1.0, 1.5, 2.0] !!) <$> randomRIO(0,2)
  x <- randomIO
  y <- randomIO
  o <- randomIO
  let newObject = GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) (Asteroid randomRadius)
  if not (checkIfObjectOverlapsWithOtherObjects newObject objects)
    then return newObject
    else do
      result <- generateGameObject (Asteroid s) objects
      return result
generateGameObject objType objects = do
  x <- randomIO
  y <- randomIO
  o <- randomIO
  let newObject = GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) objType
  if not (checkIfObjectOverlapsWithOtherObjects newObject objects)
    then return newObject
    else do
       result <- generateGameObject objType objects
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
module Generator (
    generateLevel
  ) where

import Graphics.UI.GLUT
import Control.Monad
import Datatypes
import System.Random

generateLevel :: Int -> Int -> IO GameLevel
generateLevel enemyAmount asteroidAmount = do
  enemies <- generateSeveralObjects EnemyShip enemyAmount
  asteroids <- generateSeveralObjects (Asteroid 1.0) asteroidAmount
  return (GameLevel player enemies asteroids [] [])
  where
      player = GameObject (Vector 0 0) (Vector 0 0) 0 Ship

generateSeveralObjects :: GameObjectType -> Int -> IO [GameObject]
generateSeveralObjects objType n = do
  result <- replicateM n (generateGameObject objType)
  doObjectsCollide <- checkIfObjectsOverlap result
  if doObjectsCollide
    then do
      newResult <- generateSeveralObjects objType n
      return newResult
    else return result

generateGameObject :: GameObjectType -> IO GameObject
generateGameObject (Asteroid s) = do
  randomRadius <- ([1.0, 1.5, 2.0] !!) <$> randomRIO(0,2)
  x <- randomIO
  y <- randomIO
  o <- randomIO
  return $ GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) (Asteroid randomRadius)
generateGameObject objType = do
  x <- randomIO
  y <- randomIO
  o <- randomIO
  return $ GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) objType

checkIfObjectsOverlap :: [GameObject] -> IO Bool
checkIfObjectsOverlap [] = do
  return False
checkIfObjectsOverlap (o:os) = do
  let a | length os > 0 = checkIfObjectOverlapsWithOtherObjects o os
        | otherwise = False
--  print o
--  print a
  if a
    then return True
    else if length os > 1
          then checkIfObjectsOverlap os
          else return a

checkIfObjectOverlapsWithOtherObjects :: GameObject -> [GameObject] -> Bool
checkIfObjectOverlapsWithOtherObjects o1 (o2:os)
  | d < (r1 + r2) = True
  | (length os) > 0 = checkIfObjectOverlapsWithOtherObjects o1 os
  | otherwise = False
  where
    r1 | gameObjectType o1 == EnemyShip = 0.05
       | otherwise = 0.1
    r2 | gameObjectType o2 == EnemyShip = 0.05
       | otherwise = 0.1
    dx = x (location o1) - x (location o2)
    dy = y (location o1) - y (location o2)
    dSquare = dx*dx + dy*dy
    d = sqrt dSquare
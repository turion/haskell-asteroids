module Generator (
    generateLevel
  ) where

import Graphics.UI.GLUT
import Control.Monad
import Datatypes
import System.Random

generateLevel :: Int -> Int -> IO GameLevel
generateLevel enemyAmount asteroidAmount = do
  objects <- generateSeveralObjects enemyAmount asteroidAmount
  return (GameLevel (player:objects))
  where
      player = GameObject (Vector 0 0) (Vector 0 0) 0 Ship

generateSeveralObjects :: Int -> Int -> IO [GameObject]
generateSeveralObjects 0 0 = do return []
generateSeveralObjects 0 asteroids = do
  first <- generateGameObject (Asteroid 1.0)
  rest <- generateSeveralObjects 0 (asteroids-1)
  if not (checkIfObjectOverlapsWithOtherObjects first rest)
    then return $ first:rest
    else do
      result <- generateSeveralObjects 0 asteroids
      return result
generateSeveralObjects enemies asteroids = do
  first <- generateGameObject (EnemyShip)
  rest <- generateSeveralObjects (enemies-1) asteroids
  if not (checkIfObjectOverlapsWithOtherObjects first rest)
    then return $ first:rest
    else do
      result <- generateSeveralObjects enemies asteroids
      return result

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

checkIfObjectOverlapsWithOtherObjects :: GameObject -> [GameObject] -> Bool
checkIfObjectOverlapsWithOtherObjects o1 [] = False
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
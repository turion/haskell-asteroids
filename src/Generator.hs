module Generator (
    generateLevel,
    getRandom
  ) where

import Graphics.UI.GLUT
import Control.Monad
import Datatypes
import System.Random


getRandom :: IO GLfloat
getRandom = do
  newStdGen
  g <- getStdGen
  let cc = take 1 (randoms g :: [GLfloat])
  return $ head cc

generateLevel :: Int -> Int -> IO GameLevel
generateLevel enemyAmount asteroidAmount = do
  enemies <- generateSeveralObjects EnemyShip enemyAmount
  asteroids <- generateSeveralObjects (Asteroid 1.0) asteroidAmount
  return (GameLevel player enemies asteroids [] [])
  where
      player = GameObject (Vector 0.1 0.1) (Vector 0 0) 0 Ship

generateSeveralObjects :: GameObjectType -> Int -> IO [GameObject]
generateSeveralObjects objType n = do
  result <- replicateM n (generateGameObject objType)
  return result

generateGameObject :: GameObjectType -> IO GameObject
generateGameObject (Asteroid s) = do
  randomRadius <- getRandom
  x <- getRandom
  y <- getRandom
  o <- getRandom
  let r
        | randomRadius < 0.33 = 1
        | randomRadius < 0.67 = 1.5
        | otherwise = 2
  return $ GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) (Asteroid r)
generateGameObject objType = do
  x <- getRandom
  y <- getRandom
  o <- getRandom
  return $ GameObject (Vector (x*1.9-0.95) (y*1.9-0.95)) (Vector 0 0) (o*360) objType
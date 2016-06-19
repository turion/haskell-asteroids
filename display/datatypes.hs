module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameLevel(..),
  GameObject(..)
  ) where

import Graphics.UI.GLUT

data Vector = Vector {
  x :: GLfloat,
  y :: GLfloat
  } deriving (Eq, Show)


data GameObjectType = Ship | EnemyShip | Asteroid | Projectile | EnemyProjectile
  deriving (Eq, Show)

data GameObject = GameObject {
  location :: Vector,
  velocity :: Vector,
  orientation :: GLfloat,
  scaleObject :: GLfloat,
  gameObjectType :: GameObjectType
  }

data GameLevel = GameLevel {
  player :: GameObject,
  enemies :: [GameObject],
  asteroids :: [GameObject],
  projectiles :: [GameObject],
  enemyProjectiles :: [GameObject]
}

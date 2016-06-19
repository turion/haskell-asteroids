module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameObject(..)
  ) where

import Graphics.UI.GLUT

data Vector = Vector {
  x :: GLfloat,
  y :: GLfloat
  } deriving (Eq, Show)


data GameObjectType = Ship | EnemyShip | Asteroid
  deriving (Eq, Show)

data GameObject = GameObject {
  location :: Vector,
  velocity :: Vector,
  orientation :: GLfloat,
  scaleObject :: GLfloat,
  gameObjectType :: GameObjectType
  }
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameLevel(..),
  GameObject(..),
  Shape(..),
  Location,
  Velocity,
  Acceleration,
  radius,
  Orientation,
  Scale
  ) where

import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import System.Random

data Vector = Vector {
  x :: GLfloat,
  y :: GLfloat
} deriving (Eq, Show)

data Shape = Shape {
  points :: [Vector]
} deriving (Eq, Show)

instance VectorSpace Vector GLfloat where
  zeroVector = Vector 0 0
  a *^ Vector x y = Vector (a*x) (a*y)
  Vector x1 y1 ^+^ Vector x2 y2  = Vector (x1+x2) (y1+y2)
  Vector x1 y1 `dot` Vector x2 y2 = x1*x2 + y1*y2

type Location = Vector
type Velocity = Vector
type Acceleration = GLfloat
type Orientation = GLfloat
type Scale = GLfloat

instance VectorSpace Orientation GLfloat where
  zeroVector = 0
  (*^) = (*)
  (^+^) = (+)
  dot = (*)

data GameObjectType = Ship | EnemyShip | Asteroid Scale Shape | Projectile | EnemyProjectile
   deriving (Eq, Show)

data GameObject = GameObject {
  location :: Location,
  velocity :: Velocity,
  orientation :: Orientation,
  gameObjectType :: GameObjectType
} deriving (Eq, Show)

radius :: GameObjectType -> GLfloat
radius (Asteroid scale shape)     = scale * 0.05
radius Ship                 = 0.05
radius EnemyShip            = 0.05
radius Projectile           = 0.02
radius EnemyProjectile      = 0.02

data GameLevel = GameLevel {
  objects :: [GameObject]
} deriving (Eq, Show)


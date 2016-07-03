{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameLevel(..),
  GameObject(..),
  Location,
  Velocity,
  Acceleration,
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

data GameObjectType = Ship | EnemyShip | Asteroid Scale | Projectile | EnemyProjectile
   deriving (Eq, Show)

data GameObject = GameObject {
  location :: Location,
  velocity :: Velocity,
  orientation :: Orientation,
  gameObjectType :: GameObjectType
} deriving (Eq, Show)

radius :: GameObjectType -> GLfloat
radius = radius

data GameLevel = GameLevel {
  player :: GameObject,
  enemies :: [GameObject],
  asteroids :: [GameObject],
  projectiles :: [GameObject],
  enemyProjectiles :: [GameObject]
} deriving (Eq, Show)


{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameLevel(..),
  GameObject(..),
  GameState(..),
  Shape(..),
  Fonts,
  Location,
  Velocity,
  Acceleration,
  Orientation,
  Scale,
  CollisionCorrection(..),
  Circle(..),
  CollisionResult(..)
  ) where

import Graphics.UI.GLUT
import FRP.Yampa.VectorSpace
import FRP.Yampa.Event
import Graphics.Rendering.FTGL

-- Basic types: Vector, GLfloat + types --

data Vector = Vector {
  x :: GLfloat,
  y :: GLfloat
} deriving (Eq, Show)


instance VectorSpace Vector GLfloat where
  zeroVector = Vector 0 0
  a *^ Vector x y = Vector (a*x) (a*y)
  Vector x1 y1 ^+^ Vector x2 y2  = Vector (x1+x2) (y1+y2)
  Vector x1 y1 `dot` Vector x2 y2 = x1*x2 + y1*y2

data Shape = Shape {
  points :: [Vector]
} deriving (Eq, Show)

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

-- Game types: GameObjectType, GameObject, GameLevel --

data GameObjectType = Ship | EnemyShip | Asteroid Scale Shape | Projectile | EnemyProjectile
   deriving (Eq, Show)

data GameObject = GameObject {
  location       :: Location,
  velocity       :: Velocity,
  orientation    :: Orientation,
  gameObjectType :: GameObjectType
}  deriving (Eq, Show)

data GameLevel = EmptyLevel | GameLevel {
  objects :: [GameObject]
} deriving (Eq, Show)

data GameState = GameState {
  level     :: Integer,
  lifeCount :: Integer,
  score     :: Integer
}

type Fonts = [Graphics.Rendering.FTGL.Font]

data CollisionCorrection = CollisionCorrection {
  deltaLocation :: Location,
  deltaVelocity :: Velocity
} deriving (Eq, Show)

instance VectorSpace CollisionCorrection GLfloat where
    zeroVector = CollisionCorrection (Vector 0 0) (Vector 0 0)
    a *^ CollisionCorrection loc vel = CollisionCorrection (a*^loc) (a*^vel)
    CollisionCorrection loc1 vel1 ^+^ CollisionCorrection loc2 vel2  = CollisionCorrection (loc1 ^+^ loc2) (vel1 ^+^ vel2)
    CollisionCorrection loc1 vel1 `dot` CollisionCorrection loc2 vel2 = loc1 `dot` loc2 + vel1 `dot` vel2

instance (VectorSpace v a) => VectorSpace (Event v) a where
    zeroVector = NoEvent
    a *^ NoEvent = NoEvent
    a *^ Event v = Event $ a *^ v
    NoEvent ^+^ NoEvent = NoEvent
    NoEvent ^+^ Event v = Event v
    Event v ^+^ NoEvent = Event v
    Event v ^+^ Event w = Event $ v ^+^ w
    NoEvent `dot` NoEvent = 0
    NoEvent `dot` Event v = 0
    Event v `dot` NoEvent = 0
    Event v `dot` Event w = v `dot` w

data Circle = Circle {
    circleCenter :: Location,
    circleRadius :: GLfloat
}

data CollisionResult = Correction (CollisionCorrection, CollisionCorrection) | Explosion Circle
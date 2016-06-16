module Datatypes (
  Vector(..),
  GameObjectType,
  GameObject(..),
  Location,
  Velocity,
  Acceleration,
  Orientation
  ) where

data Vector = Vector {
  x :: Double,
  y :: Double
  } deriving (Eq, Show)

type Location = Vector
type Velocity = Vector
type Acceleration = Vector
type Orientation = Double

data GameObjectType = Ship | Asteroid 
  deriving Show

data GameObject = GameObject {
  location :: Location,
  orientation :: Orientation,
  velocity :: Velocity,
  gameObjectType :: GameObjectType
  }



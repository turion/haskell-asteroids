module Datatypes (
  Vector(..),
  GameObjectType,
  GameObject(..),
  Location,
  Velocity,
  Acceleration
  ) where

data Vector = Vector {
  x :: Double,
  y :: Double
  } deriving (Eq, Show)


data GameObjectType = Ship | Asteroid 
  deriving Show

data GameObject = GameObject {
  location :: Vector,
  orientation :: Vector,
  velocity :: Vector,
  gameObjectType :: GameObjectType
  }


type Location = Vector
type Velocity = Vector
type Acceleration = Vector
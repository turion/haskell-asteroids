module Datatypes (
  Vector(..),
  GameObjectType(..),
  GameObject(..)
  ) where

data Vector = Vector {
  x :: Double,
  y :: Double
  } deriving (Eq, Show)


data GameObjectType = Ship | EnemyShip | Asteroid
  deriving Show

data GameObject = GameObject {
  location :: Vector,
  orientation :: Vector,
  velocity :: Vector,
  gameObjectType :: GameObjectType
  }
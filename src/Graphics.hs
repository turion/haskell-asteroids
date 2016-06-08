module Datatypes (
  drawGameObject
  ) where


drawGameObjectType ::   GameObjectType ->   IO ()
draw                    (Ship ship)         = return ()
draw                    (Asteroid asteroid) = return ()

drawGameObject ::   GameObject ->   IO ()
drawGameObject      gameObject      = do
  -- translate using location
  -- rotate using orientation
  -- draw using drawGameObjectType gameObject
  return ()
  
module Draw where

import Graphics.UI.GLUT
import Datatypes

drawLevel :: GameLevel -> IO ()
drawLevel (GameLevel {player = p, enemies = e, asteroids = a, projectiles = ps, enemyProjectiles = eps}) = preservingMatrix $ do
     drawGameObject p
     drawListOfGameObjects e
     drawListOfGameObjects a
     drawListOfGameObjects ps
     drawListOfGameObjects eps

drawListOfGameObjects :: [GameObject] -> IO ()
drawListOfGameObjects(x:xs) = do
  drawGameObject x
  if (length xs > 0) then drawListOfGameObjects xs else return ()

drawGameObject :: GameObject ->  IO ()
drawGameObject (GameObject {location = Vector x y, velocity = v, orientation = o, scaleObject = s, gameObjectType = objType}) = preservingMatrix $ do
     translate $ Vector3 x y 0
     rotate o $ Vector3 0 0 1
     scale s s s
     drawGameObjectType objType

drawGameObjectType Ship = do
          let x1 = 0
              y1 = 0.05
              x2 = 0.05
              y2 = -0.05
              x3 = 0
              y3 = -0.025
              x4 = -0.05
              y4 = -0.05
              c1 = 1.0
              c2 = 1.0
              c3 = 1.0 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
drawGameObjectType EnemyShip = do
          let x1 = 0
              y1 = 0.05
              x2 = 0.05
              y2 = -0.05
              x3 = 0
              y3 = -0.025
              x4 = -0.05
              y4 = -0.05
              c1 = 1.0
              c2 = 0.0
              c3 = 0.0 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
drawGameObjectType Asteroid = do
          let x1 = 0
              y1 = 0.1
              x2 = 0.1
              y2 = -0.1
              x3 = 0
              y3 = -0.15
              x4 = -0.1
              y4 = -0.1
              c1 = 0.5
              c2 = 0.5
              c3 = 0.5 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
drawGameObjectType Projectile = do
          let x1 = 0.005
              y1 = 0.02
              x2 = 0.005
              y2 = -0.02
              x3 = -0.005
              y3 = -0.02
              x4 = -0.005
              y4 = 0.02
              c1 = 0.0
              c2 = 1.0
              c3 = 0.0 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
drawGameObjectType EnemyProjectile = do
          let x1 = 0.005
              y1 = 0.025
              x2 = 0.005
              y2 = -0.025
              x3 = -0.005
              y3 = -0.025
              x4 = -0.005
              y4 = 0.025
              c1 = 1.0
              c2 = 0.3
              c3 = 0.3 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3

drawQuad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3 = do
      renderPrimitive Quads $ do
        color $ Color3 c1 c2 c3
        vertex $ Vertex2 x1 y1
        vertex $ Vertex2 x2 y2
        vertex $ Vertex2 x3 y3
        vertex $ Vertex2 x4 y4
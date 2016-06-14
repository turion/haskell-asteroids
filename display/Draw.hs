module Draw where

import Graphics.UI.GLUT
import Datatypes

drawGameObject ::   GameObject ->  IO ()
drawGameObject (GameObject {location = Vector x y, orientation = o, velocity = v, gameObjectType = objType})
      | objType == Ship          = do
          let x1 = 0
              y1 = 0.05
              x2 = 0.05
              y2 = -0.05
              x3 = 0
              y3 = -0.025
              x4 = -0.05
              y4 = -0.05 in
              renderPrimitive Quads $ do
                color $ Color3 (1.0::GLfloat) 1.0 1.0
                vertex $ Vertex2 (x1::GLfloat) (y1::GLfloat)
                vertex $ Vertex2 (x2::GLfloat) (y2::GLfloat)
                vertex $ Vertex2 (x3::GLfloat) (y3::GLfloat)
                vertex $ Vertex2 (x4::GLfloat) (y4::GLfloat)
      | objType == EnemyShip     = do
          let x1 = 0
              y1 = 0.05
              x2 = 0.05
              y2 = -0.05
              x3 = 0
              y3 = -0.025
              x4 = -0.05
              y4 = -0.05 in
              renderPrimitive Quads $ do
                color $ Color3 (1.0::GLfloat) 0.0 0.0
                vertex $ Vertex2 (x1::GLfloat) (y1::GLfloat)
                vertex $ Vertex2 (x2::GLfloat) (y2::GLfloat)
                vertex $ Vertex2 (x3::GLfloat) (y3::GLfloat)
                vertex $ Vertex2 (x4::GLfloat) (y4::GLfloat)
      | objType == Asteroid      = do
          let x1 = 0
              y1 = 0.1
              x2 = 0.1
              y2 = -0.1
              x3 = 0
              y3 = -0.15
              x4 = -0.1
              y4 = -0.1 in
              renderPrimitive Quads $ do
                color $ Color3 (0.7::GLfloat) 0.7 0.7
                vertex $ Vertex2 (x1::GLfloat) (y1::GLfloat)
                vertex $ Vertex2 (x2::GLfloat) (y2::GLfloat)
                vertex $ Vertex2 (x3::GLfloat) (y3::GLfloat)
                vertex $ Vertex2 (x4::GLfloat) (y4::GLfloat)


  {-translate (location gameObject)
  rotate (location gameObject)

  drawGameObjectType (gameObjectType gameObject)
  pops // undoes changes to transformation stack
  return ()-}
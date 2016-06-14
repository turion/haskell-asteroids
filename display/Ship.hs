module Ship where

import Graphics.UI.GLUT
import Datatypes

ship :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
ship x y w phi= do
  renderPrimitive Quads $ do
    color $ Color3 (1.0::GLfloat) 1.0 1.0
    vertex $ Vertex2 (cos(phi)*a1-sin(phi)*b1) (sin(phi)*a1 + cos(phi)*b1)
    vertex $ Vertex2 (cos(phi)*a2-sin(phi)*b2) (sin(phi)*a2 + cos(phi)*b2)
    vertex $ Vertex2 (cos(phi)*a3-sin(phi)*b3) (sin(phi)*a3 + cos(phi)*b3)
    vertex $ Vertex2 (cos(phi)*a4-sin(phi)*b4) (sin(phi)*a4 + cos(phi)*b4)
    where
        a1 = y
        b1 = w+x
        a2 = y-w
        b2 = x-w
        a3 = y
        b3 = x-w/2
        a4 = w+y
        b4 = x-w


showObject :: GameObject -> IO ()
showObject (GameObject {location = Vector x1 y1, orientation = o, velocity = v, gameObjectType = objType}) = do
    renderPrimitive Quads $ do
        color $ Color3 (1.0::GLfloat) 1.0 1.0
        vertex $ Vertex2 (x) (y)
        vertex $ Vertex2 (0.5::GLfloat) (y)
        vertex $ Vertex2 (0.5::GLfloat) (0.5::GLfloat)
        vertex $ Vertex2 (x) (0.5::GLfloat)
          where
            x = realToFrac x1
            y = realToFrac y1
            {-w = 0.1 :: GLFloat-}




{-
vertex $ Vertex2 (x) (y)
        vertex $ Vertex2 (0.5::GLfloat) (y)
        vertex $ Vertex2 (0.5::GLfloat) (0.5::GLfloat)
        vertex $ Vertex2 (x) (0.5::GLfloat)-}

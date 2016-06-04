module Asteroid where

import Graphics.UI.GLUT

asteroid :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
asteroid x y w phi= do
  renderPrimitive Quads $ do
    color $ Color3 (0.5::GLfloat) 0.5 0.5
    vertex $ Vertex2 (cos(phi)*a1-sin(phi)*b1) (sin(phi)*a1 + cos(phi)*b1)
    vertex $ Vertex2 (cos(phi)*a5-sin(phi)*b5) (sin(phi)*a5 + cos(phi)*b5)
    vertex $ Vertex2 (cos(phi)*a2-sin(phi)*b2) (sin(phi)*a2 + cos(phi)*b2)
    vertex $ Vertex2 (cos(phi)*a3-sin(phi)*b3) (sin(phi)*a3 + cos(phi)*b3)
    vertex $ Vertex2 (cos(phi)*a1-sin(phi)*b1) (sin(phi)*a1 + cos(phi)*b1)
    vertex $ Vertex2 (cos(phi)*a3-sin(phi)*b3) (sin(phi)*a3 + cos(phi)*b3)
    vertex $ Vertex2 (cos(phi)*a4-sin(phi)*b4) (sin(phi)*a4 + cos(phi)*b4)
    vertex $ Vertex2 (cos(phi)*a6-sin(phi)*b6) (sin(phi)*a6 + cos(phi)*b6)
    where
        a1 = y
        b1 = w+x
        a2 = y-w
        b2 = x-w
        a3 = y
        b3 = x-3*w/2
        a4 = w+y
        b4 = x-w
        a5 = y-3*w/2
        b5 = x+w/3
        a6 = y+w
        b6 = x+w/2


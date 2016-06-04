module Ship where

import Graphics.UI.GLUT

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

{-
     vertex $ Vertex2 y (w+x)
     vertex $ Vertex2 (y-w) (x-w)
     vertex $ Vertex2 y (x-w/2)
     vertex $ Vertex2 (w+y) (x-w)-}


module Ship where

import Graphics.UI.GLUT

ship :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
ship x y w phi= do
  renderPrimitive Quads $ do
    if phi==pi/2
        then do
             vertex $ Vertex2 (w+y) (x-w)
             vertex $ Vertex2 y (x-w/2)
             vertex $ Vertex2 (y-w) (x-w)
             vertex $ Vertex2 y (w+x)
        else  do
             vertex $ Vertex2 (w+y) (x-w)
             vertex $ Vertex2 y (x-w/2)
             vertex $ Vertex2 y (w+x)
             vertex $ Vertex2 (y-w) (x-w)



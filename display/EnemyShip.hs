module EnemyShip where

import Graphics.UI.GLUT

enemyShip :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
enemyShip x y w phi= do
  renderPrimitive Quads $ do
    color $ Color3 (1.0::GLfloat) 0.0 0.0
    vertex $ Vertex2 (cos(phi)*a2-sin(phi)*b2) (sin(phi)*a2 + cos(phi)*b2)
    vertex $ Vertex2 (cos(phi)*a1-sin(phi)*b1) (sin(phi)*a1 + cos(phi)*b1)
    vertex $ Vertex2 (cos(phi)*a4-sin(phi)*b4) (sin(phi)*a4 + cos(phi)*b4)
    vertex $ Vertex2 (cos(phi)*a3-sin(phi)*b3) (sin(phi)*a3 + cos(phi)*b3)
    where
        a1 = y
        b1 = w+x
        a2 = y-w
        b2 = x-w
        a3 = y
        b3 = x-w/2
        a4 = w+y
        b4 = x-w


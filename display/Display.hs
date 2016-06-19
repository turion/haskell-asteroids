module Display (display) where

import Graphics.UI.GLUT
import Datatypes
import Draw

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  --showObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 Ship)
  drawGameObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 20.0 2.0 Ship)
  drawGameObject (GameObject (Vector 0.4 0.4) (Vector 0 0) 200.0 1.0 EnemyShip)
  drawGameObject (GameObject (Vector (-0.5) (-0.6)) (Vector 0 0) 60.0 1.0 EnemyShip)
  drawGameObject (GameObject (Vector 0.2 (-0.8)) (Vector 0 0) 300.0 0.2 Asteroid)
  drawGameObject (GameObject (Vector (-0.8) 0.2) (Vector 0 0) 10.0 0.6 Asteroid)
  drawGameObject (GameObject (Vector 0.8 0.6) (Vector 0 0) 160.0 1.5 Asteroid)
  --drawGameObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 EnemyShip)
  --drawGameObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 Asteroid)
 {- ship 0.1 0.1 0.05 (-pi/2)
  asteroid (-0.4) 0.4 0.02 (0)
  asteroid 0.6 0.4 0.03 (pi/2)
  asteroid 0.9 0.7 0.01 (-3*pi/2)
  asteroid (-0.9) 0.7 0.02 (3*pi/2)
  enemyShip 0.4 0.4 0.03 (0)
  enemyShip 0.2 0.7 0.03 (pi/2)
  enemyShip 0.8 0.4 0.03 (-pi/2)
  enemyShip 0.4 (-0.4) 0.03 (-pi/4)
  enemyShip (-0.8) (-0.5) 0.03 (3*pi/4)-}

  --drawShip1
  flush


module Display (display) where

import Graphics.UI.GLUT
import Ship
import EnemyShip
import Asteroid
import Datatypes
import Draw

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  --ship 0.1 0.1 0.05 (-pi/2)
  --showObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 Ship)
  drawGameObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 Ship)
  drawGameObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 EnemyShip)
  drawGameObject (GameObject (Vector 0.1 0.1) (Vector 0 0) 0.0 Asteroid)
{-  asteroid (-0.4) 0.4 0.02 (0)
  asteroid 0.6 0.4 0.03 (pi/2)
  asteroid 0.9 0.7 0.01 (-3*pi/2)
  asteroid (-0.9) 0.7 0.02 (3*pi/2)
  enemyShip 0.4 0.4 0.03 (0)
  enemyShip 0.2 0.7 0.03 (pi/2)
  enemyShip 0.8 0.4 0.03 (-pi/2)
  enemyShip 0.4 (-0.4) 0.03 (-pi/4)
  enemyShip (-0.8) (-0.5) 0.03 (3*pi/4)-}
  flush


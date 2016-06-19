module Display (display) where

import Graphics.UI.GLUT
import Datatypes
import Draw

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  drawLevel (GameLevel player [enemyShip1, enemyShip2, enemyShip3] [asteroid1, asteroid2, asteroid3, asteroid4] [projectile] [enemyProjectile])
  flush
  where
    player = GameObject (Vector 0.1 0.1) (Vector 0 0) 45.0 1.5 Ship
    enemyShip1 = GameObject (Vector 0.5 0.5) (Vector 0 0) 135.0 1.0 EnemyShip
    enemyShip2 = GameObject (Vector (-0.2) 0.4) (Vector 0 0) 100.0 1.0 EnemyShip
    enemyShip3 = GameObject (Vector (-0.4) (-0.6)) (Vector 0 0) 300.0 1.0 EnemyShip
    asteroid1 = GameObject (Vector 0.7 0.8) (Vector 0 0) 200.0 1.4 Asteroid
    asteroid2 = GameObject (Vector (-0.7) 0.8) (Vector 0 0) 40.0 0.8 Asteroid
    asteroid3 = GameObject (Vector (-0.8) 0.4) (Vector 0 0) 160.0 0.5 Asteroid
    asteroid4 = GameObject (Vector (-0.6) (-0.8)) (Vector 0 0) 280.0 1.0 Asteroid
    projectile = GameObject (Vector (-0.05) 0.25) (Vector 0 0) 45.0 1.0 Projectile
    enemyProjectile = GameObject (Vector 0.4 0.4) (Vector 0 0) 135.0 1.0 EnemyProjectile


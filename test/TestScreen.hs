import Graphics.UI.GLUT
import Graphics
import Datatypes

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Haskelloids"
  displayCallback $= display
  reshapeCallback $= Just reshape
  fullScreen
  mainLoop
{- To close the fullscreen window - alt tab and right click on the item in the windows task bar and click close -}

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderLevel (GameLevel player [enemyShip1, enemyShip2, enemyShip3] [asteroid1, asteroid2, asteroid3, asteroid4] [projectile] [enemyProjectile])
  flush
  where
    player = GameObject (Vector 0.1 0.1) (Vector 0 0) 45.0 Ship
    enemyShip1 = GameObject (Vector 0.5 0.5) (Vector 0 0) 135.0 EnemyShip
    enemyShip2 = GameObject (Vector (-0.2) 0.4) (Vector 0 0) 100.0 EnemyShip
    enemyShip3 = GameObject (Vector (-0.4) (-0.6)) (Vector 0 0) 300.0 EnemyShip
    asteroid1 = GameObject (Vector 0.7 0.8) (Vector 0 0) 200.0 (Asteroid 0.8)
    asteroid2 = GameObject (Vector (-0.7) 0.8) (Vector 0 0) 40.0 (Asteroid 1.4)
    asteroid3 = GameObject (Vector (-0.8) 0.4) (Vector 0 0) 160.0 (Asteroid 0.5)
    asteroid4 = GameObject (Vector (-0.6) (-0.8)) (Vector 0 0) 280.0 (Asteroid 0.5)
    projectile = GameObject (Vector (-0.05) 0.25) (Vector 0 0) 45.0 Projectile
    enemyProjectile = GameObject (Vector 0.4 0.4) (Vector 0 0) 135.0 EnemyProjectile
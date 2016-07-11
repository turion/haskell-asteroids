import Graphics.UI.GLUT
import Graphics
import Datatypes
import Generator
import Control.Concurrent

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Haskelloids"
  fullScreen
  reshapeCallback $= Just reshape
  showText "Haskelloids" (Vector (-3) 0) [0.5, 0.0, 0.5] 0.2 0
  threadDelay 2000000
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  level <- generateLevel 10 30
  let start = GameState 1 3 0
  renderLevel level
  showGameState start
  flush
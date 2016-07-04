import Graphics.UI.GLUT
import Graphics
import Datatypes
import Generator

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Haskelloids"
  displayCallback $= display
  reshapeCallback $= Just reshape
  fullScreen
  mainLoop

display :: DisplayCallback
display = do
  level <- generateLevel 10 40
  renderLevel level
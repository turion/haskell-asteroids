import Graphics.UI.GLUT
import Graphics
import Datatypes
import Generator
import Control.Concurrent
import Data.IORef
import Data.Time.Clock

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Haskelloids"
  fullScreen
  reshapeCallback $= Just reshape
  fonts <- initFonts
  showText "Haskelloids" (Vector (-0.55) 0) [0.5, 0.0, 0.5] 0.2 fonts Title
  threadDelay 2000000
  t <- getCurrentTime
  time <- newIORef t
  displayCallback $= display time fonts
  mainLoop

display :: IORef UTCTime -> Fonts -> DisplayCallback
display time fonts = do
  level <- generateLevel 10 30
  let start = GameState 1 3 0
  renderLevel level
  showGameState start time fonts
  flush
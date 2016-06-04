import Graphics.UI.GLUT
import Bindings

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Haskelloids"
  displayCallback $= display
 {- reshapeCallback $= Just reshape-}
  keyboardMouseCallback $= Just keyboardMouse
  fullScreen
  mainLoop


{- To close the fullscreen window - alt tab and right click on the item in the windows task bar and click close -}

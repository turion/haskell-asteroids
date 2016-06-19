import Graphics.UI.GLUT
import Bindings
import Display

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Haskelloids"
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  {-if hasKeyboard==True
    then fullScreen
    else return ()-}
  fullScreen
  mainLoop

getScreenSize :: GettableStateVar Size
getScreenSize = screenSize

keyboard :: GettableStateVar Bool
keyboard = do
  hasKeyboard
  --glutInit
  --screenSize
{- To close the fullscreen window - alt tab and right click on the item in the windows task bar and click close -}

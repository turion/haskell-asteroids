module Display (display) where

import Graphics.UI.GLUT
import Ship

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  ship 0.4 0.1 0.3 (pi/2)
  flush
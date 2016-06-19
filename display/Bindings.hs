module Bindings (display, reshape, keyboardMouse) where

import Graphics.UI.GLUT
import Display
import Graphics.Rendering.OpenGL.GL.CoordTrans

reshape :: ReshapeCallback
reshape size = do
  -- print size
  viewport $= (Position 0 0, Size 900 900)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()
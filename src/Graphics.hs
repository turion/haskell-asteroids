module Graphics (
    initGL,
    reshape,
    renderLevel,
    showText,
    showGameState
  ) where

import Graphics.UI.GLUT
import Graphics.Rendering.FTGL

import Datatypes


-- Basic functions: initGL, reshape, drawPolygon --

initGL ::  IO Window
initGL     = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Haskelloids!"

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, Size 900 900)

-- expects a color vector and a list of points
drawPolygon :: (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat)] -> IO ()
drawPolygon    (r, g, b)                      points = do
    color $ Color3 r g b
    renderPrimitive Polygon $ do
        drawPoints points where
            drawPoints []              = do return ()
            drawPoints ((x, y):others) = do
                vertex $ (Vertex3   x    y  0)
                drawPoints others

-- Game functions: drawGameObjectType, drawGameObject, drawListOfGameObjects, drawGameLevel --

drawGameObjectType :: GameObjectType -> IO ()
drawGameObjectType Ship = do
    drawPolygon (1.0, 1.0, 1.0) [( 0.000,  0.050),
                                 ( 0.050, -0.050),
                                 ( 0.000, -0.025),
                                 (-0.050, -0.050)]
drawGameObjectType EnemyShip = do
    drawPolygon (1.0, 0.0, 0.0) [( 0.000,  0.050),
                                 ( 0.050, -0.050),
                                 ( 0.000, -0.025),
                                 (-0.050, -0.050)]
drawGameObjectType (Asteroid s (Shape shape))= do
    scale s s s
    drawPolygon (0.4, 0.4, 0.4) [(x vector, y vector) | vector <- shape]        --TODO test this
drawGameObjectType Projectile = do
    drawPolygon (0.0, 1.0, 0.0) [( 0.005,  0.020),
                                 ( 0.005, -0.020),
                                 (-0.005, -0.020),
                                 (-0.005,  0.020)]
drawGameObjectType EnemyProjectile = do
    drawPolygon (1.0, 0.3, 0.3) [( 0.005,  0.020),
                                 ( 0.005, -0.020),
                                 (-0.005, -0.020),
                                 (-0.005,  0.020)]

drawGameObject ::   GameObject ->   IO ()
drawGameObject      GameObject { location = location, orientation = orientation, gameObjectType = gameObjectType}     = do
    preservingMatrix $ do
        translate $ Vector3 (x location) (y location) 0
        rotate (orientation * 360 / (2 * pi)) $ Vector3 0 0 1       --degree or radians?
        drawGameObjectType gameObjectType

renderLevel :: GameLevel -> IO ()
renderLevel (GameLevel objects) = preservingMatrix $ do
     clear[ColorBuffer]
     mapM_ drawGameObject objects
     swapBuffers

--          text     position    color      scale       type (0 - title, 1 - regular)
showText :: String -> Vector -> [GLfloat] -> GLfloat -> Int -> IO ()
showText text (Vector x y) fontColors s 0 = preservingMatrix $ do
  viewport $= (Position 0 0, Size 900 900)
  color $ Color3 (fontColors !! 0) (fontColors !! 1) (fontColors !! 2)
  scale s s s
  translate $ Vector3 x y 0
  font <- createOutlineFont "FontTitle.ttf"
  setFontFaceSize font 1 1
  renderFont font text All
  swapBuffers
showText text (Vector x y) fontColors s 1 = preservingMatrix $ do
  scale s s s
  translate $ Vector3 x y 0
  color $ Color3 (fontColors !! 0) (fontColors !! 1) (fontColors !! 2)
  font <- createOutlineFont "FontNormal.ttf"
  setFontFaceSize font 1 1
  l <- createSimpleLayout
  setLayoutFont l font
  setLayoutAlignment l AlignCenter
  setLayoutLineLength l 20.0
  renderLayout l text
  swapBuffers

showGameState :: GameState -> IO()
showGameState (GameState {level = l, lifeCount = lc, score = s})  = do
  showText ("Lives: " ++ show lc ++ "              Level " ++ show l ++ "              Score: " ++ show s) (Vector (-10.0) (15.0)) [0.4, 0.8, 0.4] 0.06 1
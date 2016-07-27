module Graphics (
    initGL,
    reshape,
    renderLevel,
    showText,
    showGameState,
    drawScreen,
    TextType(..),
    initFonts
  ) where

import Graphics.UI.GLUT
import Graphics.Rendering.FTGL
import Data.IORef
import Data.Time.Clock

import Datatypes
import Generator


-- Basic functions: initGL, reshape, drawPolygon --

initGL ::  IO Window
initGL     = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Haskelloids!"

reshape :: ReshapeCallback
reshape size = do
  ss <- screenSize
  viewport $= (Position (getDistanceFromBorderToMakeScreenCentered ss) 0, modifySizeToSquare ss)

modifySizeToSquare :: Size -> Size
modifySizeToSquare (Size x y)  | x >= y    = Size y y
                               | otherwise = Size x x

getDistanceFromBorderToMakeScreenCentered :: Size -> GLsizei
getDistanceFromBorderToMakeScreenCentered (Size x y)
    | x > y     = quot (x - y) 2
    | otherwise = quot (y - x) 2

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
    drawPolygon (0.4, 0.4, 0.4) [(x vector, y vector) | vector <- shape]
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

drawScreen :: IORef GameLevel -> IORef UTCTime -> Fonts -> IORef Bool -> IO ()
drawScreen gameLevel startTime fonts resetTriggered = do
  clear[ColorBuffer]
  ilevel <- readIORef gameLevel
  resetNeeded <- readIORef resetTriggered
  if resetNeeded == True
  then do
    writeIORef resetTriggered False
    return ()
  else do
    return ()
  newLevel <- generateLevel 5 10
  let level | resetNeeded == False = ilevel
            | resetNeeded == True = newLevel
  writeIORef gameLevel level
  let start = GameState 1 3 0
  showGameState start startTime fonts
  renderLevel level

renderLevel :: GameLevel -> IO ()
renderLevel (GameLevel objects) = preservingMatrix $ do
     mapM_ drawGameObject objects
     drawBorder
     swapBuffers

drawBorder :: IO ()
drawBorder = do
  color $ Color3 (0.5 :: GLfloat) 0 0.5
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 (-a :: GLfloat) (-a)
    vertex $ Vertex2 (-a :: GLfloat) a
    vertex $ Vertex2 (a :: GLfloat) a
    vertex $ Vertex2 (a :: GLfloat) (-a)
    where a = 0.999

data TextType = Title | Regular

--          text     position    color      scale      fonts      type (0 - title, 1 - regular)
showText :: String -> Vector -> [GLfloat] -> GLfloat -> Fonts -> TextType -> IO ()
showText text (Vector x y) fontColors s fonts Title = preservingMatrix $ do
  viewport $= (Position 0 0, Size 900 900)
  color $ Color3 (fontColors !! 0) (fontColors !! 1) (fontColors !! 2)
  ss <- screenSize
  viewport $= (Position (getDistanceFromBorderToMakeScreenCentered ss) 0, modifySizeToSquare ss)
  translate $ Vector3 x y 0
  scale s s s
  renderFont (fonts !! 0) text All
showText text (Vector x y) fontColors s fonts Regular = preservingMatrix $ do
  color $ Color3 (fontColors !! 0) (fontColors !! 1) (fontColors !! 2)
  translate $ Vector3 x y 0
  scale s s s
  renderFont (fonts !! 1) text All

showGameState :: GameState -> IORef UTCTime -> Fonts -> IO()
showGameState (GameState {level = l, lifeCount = lc, score = s}) startTime fonts  = do
  now <- getCurrentTime
  before <- readIORef startTime
  let deltaTime = realToFrac $ diffUTCTime now before
  showText ("Lives: " ++ show lc ++ "   Level " ++ show l ++ "   Score: " ++ show s ++ "   Time: " ++ show deltaTime) (Vector (-0.65) (0.93)) [0.4, 0.8, 0.4] 0.06 fonts Regular

initFonts :: IO Fonts
initFonts = do
  fontNormal <- createOutlineFont "FontNormal.ttf"
  fontTitle <- createOutlineFont "FontTitle.ttf"
  setFontFaceSize fontNormal 1 1
  setFontFaceSize fontTitle 1 1
  return $ fontTitle : fontNormal : []

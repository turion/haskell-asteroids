module Graphics (
    TextType(..),
    initGL,
    reshape,
    renderLevel,
    showText,
    drawScreen
  ) where

import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock
import Data.Fixed

import Datatypes
import Generator
import Graphics.UI.GLUT.Fonts

data TextType = Title | Regular

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
                vertex $ Vertex2 x y
                drawPoints others

drawCircle :: (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat)] -> IO ()
drawCircle    (r, g, b)                      points = do
    color $ Color3 r g b
    renderPrimitive LineLoop $ do
        drawPoints points where
            drawPoints []              = do return ()
            drawPoints ((x, y):others) = do
                vertex $ Vertex2 x y
                drawPoints others

circle :: GLfloat -> [(GLfloat, GLfloat)]
circle r = [(r * sin(a * pi / 180), r * cos(a * pi / 180)) | a <- [0..360]]


-- Game functions: drawGameObjectType, drawGameObject, drawListOfGameObjects, drawGameLevel --

drawGameObjectType :: GameObjectType -> IORef GameState -> IO ()
drawGameObjectType Ship gameState = do
    state <- readIORef gameState
    drawPolygon (1.0, 1.0, 1.0) [( 0.000,  0.050),
                                 ( 0.050, -0.050),
                                 ( 0.000, -0.025),
                                 (-0.050, -0.050)]
    if (shieldOn state)==True then do
      drawCircle (0.1, 0.9, 0.9) $ circle 0.07
      drawCircle (0.1, 0.2, 0.8) $ circle 0.068
      drawCircle (0.1, 0.1, 0.7) $ circle 0.066
    else return ()
drawGameObjectType EnemyShip _ = do
    drawPolygon (1.0, 0.0, 0.0) [( 0.000,  0.050),
                                 ( 0.050, -0.050),
                                 ( 0.000, -0.025),
                                 (-0.050, -0.050)]
drawGameObjectType (Asteroid s (Shape shape) _) _= do
    scale s s s
    drawPolygon (0.4, 0.4, 0.4) [(x vector, y vector) | vector <- shape]
drawGameObjectType Projectile _ = do
    drawPolygon (0.0, 1.0, 0.0) [( 0.005,  0.020),
                                 ( 0.005, -0.020),
                                 (-0.005, -0.020),
                                 (-0.005,  0.020)]
drawGameObjectType EnemyProjectile _ = do
    drawPolygon (1.0, 0.3, 0.3) [( 0.005,  0.020),
                                 ( 0.005, -0.020),
                                 (-0.005, -0.020),
                                 (-0.005,  0.020)]

drawGameObject ::   GameObject -> IORef GameState ->  IO ()
drawGameObject  GameObject { location = location, orientation = orientation, gameObjectType = gameObjectType} gameState = do
    preservingMatrix $ do
        translate $ Vector3 (x location) (y location) 0
        rotate (orientation * 360 / (2 * pi)) $ Vector3 0 0 1       --degree or radians?
        drawGameObjectType gameObjectType gameState

drawScreen :: IORef GameState -> IORef GameLevel -> IORef UTCTime -> IO ()
drawScreen gameState gameLevel startTime = do
  clear[ColorBuffer]
  ilevel <- readIORef gameLevel
  renderLevel ilevel gameState
  showGameState gameState startTime

renderLevel :: GameLevel -> IORef GameState -> IO ()
renderLevel (GameLevel objects) gameState = preservingMatrix $ do
     mapM (\x -> drawGameObject x gameState) objects
     drawBorder

drawBorder :: IO ()
drawBorder = do
  color $ Color3 (0.5 :: GLfloat) 0 0.5
  renderPrimitive LineLoop $ do
    vertex $ Vertex2 (-a :: GLfloat) (-a)
    vertex $ Vertex2 (-a :: GLfloat) a
    vertex $ Vertex2 (a :: GLfloat) a
    vertex $ Vertex2 (a :: GLfloat) (-a)
    where a = 0.999


--          text     position     color       scale      type
showText :: String -> Vector -> [GLfloat] -> GLfloat -> TextType -> IO ()
showText text (Vector x y) fontColors s Title= preservingMatrix $ do
  color $ Color3 (fontColors !! 0) (fontColors !! 1) (fontColors !! 2)
  ss <- screenSize
  viewport $= (Position (getDistanceFromBorderToMakeScreenCentered ss) 0, modifySizeToSquare ss)
  translate $ Vector3 x y 0
  scale s s s
  renderString Roman text
  swapBuffers
showText text (Vector x y) fontColors s Regular= preservingMatrix $ do
  color $ Color3 (fontColors !! 0) (fontColors !! 1) (fontColors !! 2)
  translate $ Vector3 x y 0
  scale s s s
  renderString Roman text
  swapBuffers

showGameState :: IORef GameState -> IORef UTCTime -> IO()
showGameState gameState startTime  = do
  now <- getCurrentTime
  gs <- readIORef gameState
  let newShields | shields gs + 1 > 100 = 100
                 | shieldOn gs == True = shields gs
                 | otherwise = shields gs + 1
  writeIORef gameState $ GameState (level gs) (lifeCount gs) (score gs) newShields (shieldOn gs)
  before <- readIORef startTime
  let deltaTime = realToFrac $ diffUTCTime now before
  showText ("Lives: " ++ show (lifeCount gs) ++ " Level " ++ show (level gs) ++ " Score: " ++ show (score gs) ++ " Shields:" ++ show (shields gs) ++ " Time: " ++ show deltaTime) (Vector (-0.92) (0.9)) [0.4, 0.8, 0.4] 0.0005 Regular

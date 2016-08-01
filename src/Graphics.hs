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
        drawPoints points

drawPoints :: [(GLfloat, GLfloat)] -> IO ()
drawPoints []              = do return ()
drawPoints ((x, y):others) = do
    vertex $ Vertex2 x y
    drawPoints others

drawCircle :: (GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat)] -> IO ()
drawCircle    (r, g, b)                      points = do
    color $ Color3 r g b
    renderPrimitive LineLoop $ do
        drawPoints points

circle :: GLfloat -> [(GLfloat, GLfloat)]
circle r = [(r * sin(a * pi / 180), r * cos(a * pi / 180)) | a <- [0..360]]


-- Game functions: drawGameObjectType, drawGameObject, drawListOfGameObjects, drawGameLevel --

drawGameObjectType :: GameObjectType -> IORef GameState -> IO ()
drawGameObjectType Ship gameState = do
    state <- readIORef gameState
    drawPolygon (0.75, 0.75, 0.75) $ getShipForm shipSize
    drawPolygon (0.1, 0.3, 0.9) $ getWindowForm shipSize
    drawPolygon (0.1, 0.6, 0.2) $ getRightWingForm shipSize
    drawPolygon (0.1, 0.6, 0.2) $ getLeftWingForm shipSize
    if (shields state < 500) == True && (shieldOn state) == False then do
      drawPolygon (0.9, 0.2, 0.2) $ getLightningForm (shipSize * 2)
    else return ()
    if (shieldOn state) == True then do
      drawCircle (0.1, 0.9, 0.9) $ circle (shipSize - 0.005)
      drawCircle (0.1, 0.2, 0.8) $ circle (shipSize - 0.007)
      drawCircle (0.1, 0.1, 0.7) $ circle (shipSize - 0.009)
    else return ()
    where shipSize = 0.065
drawGameObjectType EnemyShip _ = do
    drawPolygon (0.65, 0.65, 0.65) $ getEnemyShipForm enemyShipSize
    drawPolygon (0.1, 0.3, 0.9) $ getWindowEnemyForm enemyShipSize
    drawPolygon (0.65, 0.65, 0.65) $ getRightWingEnemyForm enemyShipSize
    drawPolygon (0.9, 0.1, 0.1) $ getRightWingEnemyColorForm enemyShipSize
    drawPolygon (0.65, 0.65, 0.65) $ getLeftWingEnemyForm enemyShipSize
    drawPolygon (0.9, 0.1, 0.1) $ getLeftWingEnemyColorForm enemyShipSize
    where enemyShipSize = 0.065
drawGameObjectType (Asteroid s (Shape shape) _) _= do
    scale s s s
    drawPolygon (0.3, 0.3, 0.3) [(x vector, y vector) | vector <- shape]
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
    swapBuffers


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

showGameState :: IORef GameState -> IORef UTCTime -> IO()
showGameState gameState startTime  = do
    now <- getCurrentTime
    gs <- readIORef gameState
    let newShields | shields gs + 1 > 1000 = 1000
                   | shieldOn gs == True = shields gs
                   | otherwise = shields gs + 2
    writeIORef gameState $ GameState (level gs) (lifeCount gs) (score gs) newShields (shieldOn gs)
    before <- readIORef startTime
    let deltaTime = realToFrac $ diffUTCTime now before
    showText ("Lives: " ++ show (lifeCount gs) ++ "   Level " ++ show (level gs) ++ "   Score: " ++ show (score gs) ++ "   Time: " ++ show deltaTime) (Vector (-0.85) (0.9)) [0.4, 0.8, 0.4] 0.0005 Regular

getShipForm :: GLfloat -> [(GLfloat, GLfloat)]
getShipForm s = recalculateCoords s $ copyAndReverse [(200,50), (210, 60), (215, 45), (230, 100), (245, 80), (245, 160), (345, 260), (350, 245), (350, 300), (250, 300), (240, 335), (210, 335), (200, 300)]

getEnemyShipForm :: GLfloat -> [(GLfloat, GLfloat)]
getEnemyShipForm s = recalculateCoords s $ copyAndReverse [(200,50), (210, 50), (240, 160), (260, 160), (285, 185), (370, 210), (370, 240), (250, 270), (240, 320), (200, 310), (200, 310), (200, 200)]

getWindowForm :: GLfloat -> [(GLfloat, GLfloat)]
getWindowForm s = recalculateCoords s $ copyAndReverse [(200, 100), (220, 120), (220, 160), (200, 160)]

getWindowEnemyForm :: GLfloat -> [(GLfloat, GLfloat)]
getWindowEnemyForm s = recalculateCoords s $ copyAndReverse [(200, 140), (220, 140), (230, 170), (200, 170)]

getRightWingForm :: GLfloat -> [(GLfloat, GLfloat)]
getRightWingForm s = recalculateCoords s [(245, 250), (245, 210), (295, 250)]

getLeftWingForm :: GLfloat -> [(GLfloat, GLfloat)]
getLeftWingForm s = recalculateCoords s [(155, 250), (155, 210), (105, 250)]

getRightWingEnemyForm :: GLfloat -> [(GLfloat, GLfloat)]
getRightWingEnemyForm s = recalculateCoords s [(360, 190), (370, 190), (370, 250), (360, 250)]

getLeftWingEnemyForm :: GLfloat -> [(GLfloat, GLfloat)]
getLeftWingEnemyForm s = recalculateCoords s [(40, 190), (30, 190), (30, 250), (40, 250)]

getRightWingEnemyColorForm :: GLfloat -> [(GLfloat, GLfloat)]
getRightWingEnemyColorForm s = recalculateCoords s [(302, 190), (338, 200), (338, 248), (302, 257)]

getLeftWingEnemyColorForm :: GLfloat -> [(GLfloat, GLfloat)]
getLeftWingEnemyColorForm s = recalculateCoords s [(98, 190), (62, 200), (62, 248), (98, 257)]

recalculateCoords :: GLfloat -> [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)]
recalculateCoords s list = [ ((x - 200) * s/200, -(y - 200) * s/200) | (x, y) <- list]

getLightningForm :: GLfloat -> [(GLfloat, GLfloat)]
getLightningForm s = recalculateCoords s [(255, 125), (253, 128), (240, 130), (270, 95), (256, 123), (270, 120), (240, 160)]

copyAndReverse :: [(GLfloat, GLfloat)] -> [(GLfloat, GLfloat)]
copyAndReverse list = reversedList ++ list
  where
    reversedList = reverse [ (400 - x, y) | (x, y) <- list]


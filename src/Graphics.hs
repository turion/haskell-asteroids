module Graphics (
    initGL,
    reshape,
    renderLevel
  ) where

import Graphics.UI.GLUT

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
drawGameObjectType (Asteroid s)= do
    scale s s s
    drawPolygon (0.4, 0.4, 0.4) [( 0.000,  0.050),
                                 ( 0.040,  0.030),
                                 ( 0.030,  0.040),
                                 ( 0.050,  0.000),
                                 ( 0.030, -0.040),
                                 ( 0.040, -0.030),
                                 ( 0.000, -0.050),
                                 (-0.040, -0.030),
                                 (-0.030, -0.040),
                                 (-0.050,  0.000),
                                 (-0.050,  0.000),
                                 (-0.040,  0.030)]
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

drawListOfGameObjects :: [GameObject] -> IO ()
drawListOfGameObjects = mapM_ drawGameObject

renderLevel :: GameLevel -> IO ()
renderLevel (GameLevel {player = p, enemies = e, asteroids = a, projectiles = ps, enemyProjectiles = eps}) = preservingMatrix $ do
     clear[ColorBuffer]
     drawGameObject p
     drawListOfGameObjects e
     drawListOfGameObjects a
     drawListOfGameObjects ps
     drawListOfGameObjects eps
     swapBuffers
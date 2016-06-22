module Graphics (
    initGL,
    reshape,
    renderLevel
  ) where

import Graphics.UI.GLUT

import Datatypes

initGL ::  IO ()
initGL     = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Haskelloids!"
    return ()

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, Size 900 900)

drawGameObjectType Ship = do
          let x1 = 0
              y1 = 0.05
              x2 = 0.05
              y2 = -0.05
              x3 = 0
              y3 = -0.025
              x4 = -0.05
              y4 = -0.05
              c1 = 1.0
              c2 = 1.0
              c3 = 1.0 in
              drawQuad x2 y2 x1 y1 x4 y4 x3 y3 c1 c2 c3
drawGameObjectType EnemyShip = do
          let x1 = 0
              y1 = 0.05
              x2 = 0.05
              y2 = -0.05
              x3 = 0
              y3 = -0.025
              x4 = -0.05
              y4 = -0.05
              c1 = 1.0
              c2 = 0.0
              c3 = 0.0 in
              drawQuad x2 y2 x1 y1 x4 y4 x3 y3 c1 c2 c3
drawGameObjectType (Asteroid s)= do
  renderPrimitive Polygon $ do
            color $ Color3 (0.4 :: GLfloat) 0.4 0.4
            vertex $ (Vertex3   0.00    0.05  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.04    0.03  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.03    0.04  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.05    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.03  (-0.04) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.04  (-0.03) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00  (-0.05) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.04) (-0.03) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.03) (-0.04) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.05)   0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.03)   0.04  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.04)   0.03  0 :: Vertex3 GLfloat)
drawGameObjectType Projectile = do
          let x1 = 0.005
              y1 = 0.02
              x2 = 0.005
              y2 = -0.02
              x3 = -0.005
              y3 = -0.02
              x4 = -0.005
              y4 = 0.02
              c1 = 0.0
              c2 = 1.0
              c3 = 0.0 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
drawGameObjectType EnemyProjectile = do
          let x1 = 0.005
              y1 = 0.025
              x2 = 0.005
              y2 = -0.025
              x3 = -0.005
              y3 = -0.025
              x4 = -0.005
              y4 = 0.025
              c1 = 1.0
              c2 = 0.3
              c3 = 0.3 in
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3

drawQuad :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3 = do
      renderPrimitive Quads $ do
        color $ Color3 c1 c2 c3
        vertex $ Vertex2 x1 y1
        vertex $ Vertex2 x2 y2
        vertex $ Vertex2 x3 y3
        vertex $ Vertex2 x4 y4

drawGameObject ::   GameObject ->   IO ()
drawGameObject      GameObject { location = location, orientation = orientation, gameObjectType = gameObjectType}     = do
    preservingMatrix $ do
        translate $ (Vector3 (x location) (y location) 0)
        rotate (orientation * 360 / (2 * pi)) $ Vector3 0 0 1       --degree or radians?
        drawGameObjectType $ gameObjectType

renderLevel :: GameLevel -> IO ()
renderLevel (GameLevel {player = p, enemies = e, asteroids = a, projectiles = ps, enemyProjectiles = eps}) = preservingMatrix $ do
     clear[ColorBuffer]
     drawGameObject p
     drawListOfGameObjects e
     drawListOfGameObjects a
     drawListOfGameObjects ps
     drawListOfGameObjects eps
     swapBuffers

drawListOfGameObjects :: [GameObject] -> IO ()
drawListOfGameObjects = mapM_ drawGameObject
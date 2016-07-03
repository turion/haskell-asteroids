module Graphics (
    initGL,
    renderLevel,
    reshape
  ) where

import Graphics.UI.GLUT
import Control.Monad
import Datatypes
import Generator
import System.Random

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
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
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
              drawQuad x1 y1 x2 y2 x3 y3 x4 y4 c1 c2 c3
drawGameObjectType (Asteroid s)= do
  scale s s s
  renderPrimitive Polygon $ do
            y1 <- randomIO
            x2 <- randomIO
            x3 <- randomIO
            x4 <- randomIO
            y5 <- randomIO
            x6 <- randomIO
            x7 <- randomIO
            x8 <- randomIO
            color $ Color3 (0.4 :: GLfloat) 0.4 0.4
            vertex $ (Vertex2   0               (y1 * a + a)    :: Vertex2 GLfloat)
            vertex $ (Vertex2   (x2 * b + b)    (x2 * b + b)    :: Vertex2 GLfloat)
            vertex $ (Vertex2   (x3 * a + a)    0               :: Vertex2 GLfloat)
            vertex $ (Vertex2   (x4 * b + b)    (-(x4 * b + b)) :: Vertex2 GLfloat)
            vertex $ (Vertex2   0               (-(y5 * a + a)) :: Vertex2 GLfloat)
            vertex $ (Vertex2   (-(x6 * b + b)) (-(x6 * b + b)) :: Vertex2 GLfloat)
            vertex $ (Vertex2   (-(x7 * a + a)) 0               :: Vertex2 GLfloat)
            vertex $ (Vertex2   (-(x8 * b + b)) (x8 * b + b)    :: Vertex2 GLfloat)
            where a = 0.025
                  b = 0.0175
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
renderLevel (GameLevel objects) = preservingMatrix $ do
     clear[ColorBuffer]
     mapM_ drawGameObject objects
     swapBuffers
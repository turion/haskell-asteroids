module Graphics (
    initGL,
    renderGameObject
  ) where

import Graphics.UI.GLUT

import Datatypes

initGL ::  IO ()
initGL     = do
    getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Haskelloids!"
    return ()

drawGameObjectType :: GameObjectType ->   IO ()
drawGameObjectType    Ship         = do
    renderPrimitive Polygon $ do
            vertex $ (Vertex3   0.00    0.05  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.02  (-0.02) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.02) (-0.02) 0 :: Vertex3 GLfloat)
drawGameObjectType    Asteroid = do
    renderPrimitive Polygon $ do
            vertex $ (Vertex3   0.00    0.05  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.02    0.03  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.03    0.02  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.05    0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.03  (-0.02) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.02  (-0.03) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3   0.00  (-0.05) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.02) (-0.03) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.03) (-0.02) 0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.05)   0.00  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.03)   0.02  0 :: Vertex3 GLfloat)
            vertex $ (Vertex3 (-0.02)   0.03  0 :: Vertex3 GLfloat)


renderGameObject ::   GameObject ->   IO ()
renderGameObject      gameObject      = do
    clear[ColorBuffer]
    preservingMatrix $ do
        let location = getLocation gameObject
        translate $ (Vector3 (realToFrac (getX location):: GLfloat) (realToFrac (getY location):: GLfloat) 0)
        let orientation = getOrientation gameObject
        rotate (realToFrac orientation * 360 / (2 * pi) :: GLfloat) $ Vector3 0 0 1
        drawGameObjectType $ getGameObjectType gameObject
    swapBuffers
  
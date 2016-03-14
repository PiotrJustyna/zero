module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

main :: IO ()
main = do
    getArgsAndInitialize
    createAWindow "zero"
    mainLoop

createAWindow :: String -> IO ()
createAWindow windowName = do
    createWindow windowName
    displayCallback $= displayPoints

displayPoints :: IO ()
displayPoints = do
    clear [ColorBuffer]
    renderPrimitive
        Polygon
        $mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints =
    [(0.75, -0.75, 0.0)
    ,(0.0, 0.75, 0.0)
    ,(-0.75, -0.75, 0.0)]

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
        Triangles
        $mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (foldl (\acc x -> (myPoints x) ++ acc) [(0.0, 0.0, 0.0)] startingPoints)

startingPoints :: [(Float, Float)]
startingPoints = [(x, y) | x <- [-0.95, -0.9 .. 0.9], y <- [-0.95, -0.9 .. 0.9]]

myPoints :: (Float, Float) -> [(GLfloat, GLfloat, GLfloat)]
myPoints coordinates = [((fst coordinates) + 0.0075, (snd coordinates) - 0.0075, 0.0), ((fst coordinates) + 0.0, (snd coordinates) + 0.0075, 0.0) ,((fst coordinates) - 0.0075, (snd coordinates) - 0.0075, 0.0)]

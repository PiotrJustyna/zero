{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Player
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)

defaultZeroContextFactory :: ContextFactory c ds GLFW.GLFWWindow
defaultZeroContextFactory = GLFW.newContext' [] (GLFW.WindowConf 800 600 "zero")

projectPlayerLocation :: PlayerLocation -> [(V4 Float, V3 Float)]
projectPlayerLocation (V3 playerLocationX playerLocationY playerLocationZ) =
    [((V4 (playerLocationX + 0.04) (playerLocationY - 0.04) playerLocationZ 1), V3 0 1 0),
    ((V4 (playerLocationX + 0.0) (playerLocationY + 0.04) playerLocationZ 1), V3 0 1 0),
    ((V4 (playerLocationX - 0.04) (playerLocationY - 0.04) playerLocationZ 1), V3 0 1 0)]

players =   [Player "a" 100 (V3 (-0.5) 0 0),
            Player "b" 100 (V3 0 0 0),
            Player "c" 100 (V3 0.5 0 0)]

main =
    runContextT defaultZeroContextFactory (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (3 * (length players))
        writeBuffer vertexBuffer 0 (foldl (\acc x -> (projectPlayerLocation (getLocation x)) ++ acc) ([] :: [(V4 Float, V3 Float)]) players)

        shader <- compileShader $ do
            primitiveStream <- toPrimitiveStream id
            fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 600), DepthRange 0 1)) primitiveStream
            drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream

        loop vertexBuffer shader

loop vertexBuffer shader = do
    render $ do
        clearContextColor (V3 0.2 0.2 0.2)
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
    swapContextBuffers

    closeRequested <- GLFW.windowShouldClose
    unless closeRequested $ loop vertexBuffer shader

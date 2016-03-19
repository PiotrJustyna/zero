{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)

defaultZeroContextFactory :: ContextFactory c ds GLFW.GLFWWindow
defaultZeroContextFactory = GLFW.newContext' [] (GLFW.WindowConf 800 600 "zero")

main =
    runContextT defaultZeroContextFactory (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
        writeBuffer vertexBuffer 0 [(V4 0.8 (-0.8) 0 1, V3 1 0 0),
                                    (V4 0 0.8 0 1, V3 0 1 0),
                                    (V4 (-0.8) (-0.8) 0 1, V3 0 0 1)]

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

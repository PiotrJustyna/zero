{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)

main =
    runContextT GLFW.newContext (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer 3
        writeBuffer vertexBuffer 0 [ (V4 (-0.8) (-0.8) 0 1, V3 0 0 1)
                                   , (V4 0 0.8 0 1, V3 0 1 0)
                                   , (V4 0.8 (-0.8) 0 1,  V3 1 0 0)
                                   ]

        shader <- compileShader $ do
            primitiveStream <- toPrimitiveStream id
            fragmentStream <- rasterize (const (FrontAndBack, ViewPort (V2 0 0) (V2 1024 768), DepthRange 0 1)) primitiveStream
            drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream

        loop vertexBuffer shader

loop vertexBuffer shader = do
    render $ do
        clearContextColor (V3 0 0 0)
        vertexArray <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
    swapContextBuffers

    closeRequested <- GLFW.windowShouldClose
    unless closeRequested $ loop vertexBuffer shader

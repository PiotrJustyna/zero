{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Player
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)

defaultZeroContextFactory :: ContextFactory c ds GLFW.GLFWWindow
defaultZeroContextFactory = GLFW.newContext' [] (GLFW.WindowConf 800 600 "zero")

projectPlayer :: Player -> [(V4 Float, V3 Float)]
projectPlayer (Player playerName playerHitPoints (V3 x y z)) =
    [((V4 (x + 0.05) (y - 0.05) z 1), V3 redChannel greenChannel 0),
    ((V4 (x + 0.0) (y + 0.05) z 1), V3 redChannel greenChannel 0),
    ((V4 (x - 0.05) (y - 0.05) z 1), V3 redChannel greenChannel 0)]
    where
    greenChannel = (fromIntegral playerHitPoints) / 100.0 :: Float
    redChannel = (1.0 :: Float) - (fromIntegral playerHitPoints) / 100.0 :: Float

players =   [Player "a" 0 (V3 (-0.5) 0 0),
            Player "b" 20 (V3 0 0 0),
            Player "c" 40 (V3 0.5 0 0),
            Player "d" 60 (V3 (-0.25) 0.25 0),
            Player "e" 80 (V3 0.25 0.25 0)]

main =
    runContextT defaultZeroContextFactory (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (3 * (length players))
        writeBuffer vertexBuffer 0 (foldl (\acc x -> (projectPlayer x) ++ acc) ([] :: [(V4 Float, V3 Float)]) players)

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

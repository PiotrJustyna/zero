{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Player
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Monad (unless)
import Control.Arrow (first)

defaultZeroContextFactory :: ContextFactory c ds GLFW.GLFWWindow
defaultZeroContextFactory = GLFW.newContext' [] (GLFW.WindowConf 800 600 "zero")

projectPlayer :: Player -> [(V4 Float, V3 Float)]
projectPlayer (Player playerName playerHitPoints (V3 x y z)) =
    [((V4 (x + 0.05) (y - 0.05) z 1), V3 redChannel greenChannel 0),
    ((V4 (x + 0.0) (y + 0.05) z 1), V3 redChannel greenChannel 0),
    ((V4 (x - 0.05) (y - 0.05) z 1), V3 redChannel greenChannel 0)]
    where
    greenChannel = (fromIntegral playerHitPoints) / 100.0
    redChannel = 1.0 - (fromIntegral playerHitPoints) / 100.0

players :: [Player]
players = [Player "a" 0 (V3 (-0.5) 0 0),
            Player "b" 20 (V3 0 0 0),
            Player "c" 40 (V3 0.5 0 0),
            Player "d" 60 (V3 (-0.25) 0.25 0),
            Player "e" 80 (V3 0.25 0.25 0)]

rotationMatrix :: S V Float -> V4 (V4 VFloat)
rotationMatrix a = V4 row1 row2 row3 row4
    where
        row1 = V4 (cos a) (-sin a) 0 0
        row2 = V4 (sin a) (cos a) 0 0
        row3 = V4 0 0 1 0
        row4 = V4 0 0 0 1

main =
    runContextT defaultZeroContextFactory (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (3 * (length players))
        uniformBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1
        writeBuffer vertexBuffer 0 (foldl (\acc x -> (projectPlayer x) ++ acc) ([] :: [(V4 Float, V3 Float)]) players)
        shader :: CompiledShader os (ContextFormat RGBFloat ()) (PrimitiveArray Triangles (B4 Float, B3 Float)) <- compileShader $ do
            initialPrimitiveStream :: PrimitiveStream Triangles (VertexFormat(B4 Float, B3 Float)) <- toPrimitiveStream id
            uniform :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 0))
            let rotatedPrimitiveStream :: PrimitiveStream Triangles (VertexFormat(B4 Float, B3 Float)) = fmap (first (rotationMatrix uniform !*)) initialPrimitiveStream
            let shiftedPrimitiveStream :: PrimitiveStream Triangles (VertexFormat(B4 Float, B3 Float)) = fmap (\(position, color) -> (position - V4 0.3 0.3 0 0, color / 2)) rotatedPrimitiveStream
            let combinedPrimitiveStreams :: PrimitiveStream Triangles (VertexFormat(B4 Float, B3 Float)) = mappend initialPrimitiveStream shiftedPrimitiveStream
            fragmentStream :: FragmentStream (V3 (FragmentFormat (S V Float))) <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 600), DepthRange 0 1)) combinedPrimitiveStreams
            drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream
        loop vertexBuffer shader uniformBuffer 0.0

loop :: Buffer os (B4 Float, B3 Float)
    -> CompiledShader os (ContextFormat RGBFloat ()) (PrimitiveArray Triangles (B4 Float, B3 Float))
    -> Buffer os (Uniform (B Float))
    -> Float
    -> ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat ()) IO ()
loop vertexBuffer shader uniformBuffer angle = do
    writeBuffer uniformBuffer 0 [angle]
    render $ do
        clearContextColor (V3 0.2 0.2 0.2)
        vertexArray :: VertexArray () (B4 Float, B3 Float) <- newVertexArray vertexBuffer
        let primitiveArray :: PrimitiveArray Triangles (B4 Float, B3 Float) = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
    swapContextBuffers

    closeRequested :: Bool <- GLFW.windowShouldClose
    unless closeRequested $ loop vertexBuffer shader uniformBuffer ((angle+0.01) `mod''` (2*pi))

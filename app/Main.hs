{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
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
    greenChannel = (fromIntegral playerHitPoints) / 100.0 :: Float
    redChannel = (1.0 :: Float) - (fromIntegral playerHitPoints) / 100.0 :: Float

players :: [Player]
players =   [Player "a" 0 (V3 (-0.5) 0 0),
            Player "b" 20 (V3 0 0 0),
            Player "c" 40 (V3 0.5 0 0),
            Player "d" 60 (V3 (-0.25) 0.25 0),
            Player "e" 80 (V3 0.25 0.25 0)]

main =
    runContextT defaultZeroContextFactory (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (3 * (length players))
        writeBuffer vertexBuffer 0 (foldl (\acc x -> (projectPlayer x) ++ acc) ([] :: [(V4 Float, V3 Float)]) players)

        uniformBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 1

        shader <- compileShader $ do
            primitiveStream <- toPrimitiveStream id
            let rotationMatrix a =  V4 (V4 (cos a) (-sin a) 0 0)
                                    (V4 (sin a) (cos a) 0 0)
                                    (V4 0    0    1 0)
                                    (V4 0    0    0 1)
            uniform <- getUniform (const (uniformBuffer,0))
            let primitiveStream2 = fmap (first (rotationMatrix uniform !*)) primitiveStream
            let primitiveStream3 = fmap (\(pos,clr) -> (pos - V4 0.3 0.3 0 0, clr / 2)) primitiveStream2
            let primitiveStream4 = primitiveStream `mappend` primitiveStream3
            fragmentStream <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 600), DepthRange 0 1)) primitiveStream4
            drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream

        loop vertexBuffer shader uniformBuffer 0.0

loop vertexBuffer shader uniformBuffer angle = do
    writeBuffer uniformBuffer 0 [angle]
    render $ do
        clearContextColor (V3 0.2 0.2 0.2)
        -- This is quite bizarre to me:
        -- vertexArray :: VertexArray () (B4 Float, B3 Float)
        -- According to the 'data VertexArray t a' documentation:
        -- A vertex array is the basic building block for a primitive array.
        -- It is created from the contents of a Buffer, but unlike a Buffer,
        -- it may be truncated, zipped with other vertex arrays,
        -- and even morphed into arrays of a different type with the provided Functor instance.
        -- A VertexArray t a has elements of type a, and
        -- t indicates whether the vertex array may be used as instances or not.
        -- The last line is what's puzzling at the moment: compiler insisted I make 't' a '()' unit -
        -- I believe it means the vertexArray cannot (will not?) be used as instances (an instance?).
        -- The type is explicitly declared, but not sure if this is correct.
        vertexArray :: VertexArray () (B4 Float, B3 Float) <- newVertexArray vertexBuffer
        let primitiveArray = toPrimitiveArray TriangleList vertexArray
        shader primitiveArray
    swapContextBuffers

    closeRequested <- GLFW.windowShouldClose
    unless closeRequested $ loop vertexBuffer shader uniformBuffer ((angle+0.1) `mod''` (2*pi))

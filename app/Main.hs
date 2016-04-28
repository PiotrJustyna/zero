{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
module Main where

import Camera
import Tetrahedron
import Representation
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import Control.Arrow (first)
import Control.Monad (unless)

defaultZeroContextFactory :: ContextFactory c ds GLFW.GLFWWindow
defaultZeroContextFactory = GLFW.newContext' [] (GLFW.WindowConf 800 600 "zero")

objects :: [Tetrahedron]
objects = [Tetrahedron]

lightPosition :: V4 VFloat
lightPosition = V4 0.0 2.0 0.0 0.0

main =
    runContextT defaultZeroContextFactory (ContextFormatColor RGB8) $ do
        vertexBuffer :: Buffer os (B4 Float, B3 Float) <- newBuffer (((length $ asLines (head objects)) * 3 * (length objects)))
        uniformBuffer :: Buffer os (Uniform (B Float)) <- newBuffer 6
        let representationOfObjects = (foldl (\acc x -> (asLines x) ++ acc) ([] :: [(V4 Float, V3 Float)]) objects)
        writeBuffer vertexBuffer 0 representationOfObjects
        let rawNormals1 = calculateRawNormalsForLines representationOfObjects
        writeBuffer vertexBuffer ((length $ asLines (head objects)) * (length objects)) (zipModelVerticesAndNormalVertices representationOfObjects rawNormals1)
        shader :: CompiledShader os (ContextFormat RGBFloat ()) ((PrimitiveArray Lines (B4 Float, B3 Float)), (PrimitiveArray Lines (B4 Float, B3 Float))) <- compileShader $ do
            initialPrimitiveStream :: PrimitiveStream Lines (VertexFormat (B4 Float, B3 Float)) <- toPrimitiveStream (\x -> fst x)
            normalPrimitiveStream :: PrimitiveStream Lines (VertexFormat (B4 Float, B3 Float)) <- toPrimitiveStream (\x -> snd x)
            rX :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 0))
            rY :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 1))
            rZ :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 2))
            tX :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 3))
            tY :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 4))
            tZ :: UniformFormat (B Float) V <- getUniform (const (uniformBuffer, 5))
            let transformedPrimitiveStream :: PrimitiveStream Lines (VertexFormat(B4 Float, B3 Float)) = (\(vertex, colour) -> (mvpMatrix rX rY rZ tX tY tZ !* vertex, colour / 2)) <$> initialPrimitiveStream
            let transformedNormalPrimitiveStream :: PrimitiveStream Lines (VertexFormat(B4 Float, B3 Float)) = (first (mvpMatrix rX rY rZ tX tY tZ !*)) <$> normalPrimitiveStream
            let combinedPrimitiveStreams = transformedPrimitiveStream `mappend` transformedNormalPrimitiveStream
            fragmentStream :: FragmentStream (V3 (FragmentFormat (S V Float))) <- rasterize (const (Front, ViewPort (V2 0 0) (V2 800 600), DepthRange 0 1)) combinedPrimitiveStreams
            drawContextColor (const (ContextColorOption NoBlending (V3 True True True))) fragmentStream
        loop vertexBuffer shader uniformBuffer [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

loop :: Buffer os (B4 Float, B3 Float)
    -> CompiledShader os (ContextFormat RGBFloat ()) ((PrimitiveArray Lines (B4 Float, B3 Float)), (PrimitiveArray Lines (B4 Float, B3 Float)))
    -> Buffer os (Uniform (B Float))
    -> [Float]
    -> ContextT GLFW.GLFWWindow os (ContextFormat RGBFloat ()) IO ()
loop vertexBuffer shader uniformBuffer transformations = do
    writeBuffer uniformBuffer 0 transformations
    render $ do
        clearContextColor (V3 0.2 0.2 0.2)
        vertexArray :: VertexArray () (B4 Float, B3 Float) <- newVertexArray vertexBuffer
        let primitiveArray :: PrimitiveArray Lines (B4 Float, B3 Float) = toPrimitiveArray LineList (takeVertices ((length $ asLines (head objects)) * (length objects)) vertexArray)
        let normalArray :: PrimitiveArray Lines (B4 Float, B3 Float) = toPrimitiveArray LineList (dropVertices ((length $ asLines (head objects)) * (length objects)) vertexArray)
        shader (primitiveArray, normalArray)
    swapContextBuffers

    rotateXKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'Q
    rotateYKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'W
    rotateZKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'E
    translateXKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'A
    translateYKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'S
    translateZKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'D
    reverseKeyState :: GLFW.KeyState <- GLFW.getKey GLFW.Key'RightShift
    closeRequested :: Bool <- GLFW.windowShouldClose

    unless closeRequested $ loop vertexBuffer shader uniformBuffer
        [(((head transformations) + extractInputValue rotateXKeyState reverseKeyState) `mod''` (2 * pi)),
        (((transformations !! 1) + extractInputValue rotateYKeyState reverseKeyState) `mod''` (2 * pi)),
        (((transformations !! 2) + extractInputValue rotateZKeyState reverseKeyState) `mod''` (2 * pi)),
        (transformations !! 3) + extractInputValue translateXKeyState reverseKeyState,
        (transformations !! 4) + extractInputValue translateYKeyState reverseKeyState,
        (transformations !! 5) + extractInputValue translateZKeyState reverseKeyState]

extractInputValue :: GLFW.KeyState -> GLFW.KeyState -> Float
extractInputValue actionKeyState reverseKeyState
    | actionKeyState == GLFW.KeyState'Pressed && reverseKeyState == GLFW.KeyState'Released = 0.01
    | actionKeyState == GLFW.KeyState'Pressed && reverseKeyState == GLFW.KeyState'Pressed = -0.01
    | otherwise = 0

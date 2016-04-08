module Representation where

import Graphics.GPipe

class Projection a where
    asPoints :: a -> [(V4 Float, V3 Float)]
    numberOfPointVertices :: a -> Int

    asLines :: a -> [(V4 Float, V3 Float)]
    numberOfLineVertices :: a -> Int

    asTriangles :: a -> [(V4 Float, V3 Float)]
    numberOfTriangleVertices :: a -> Int

module Representation where

import Graphics.GPipe

class Representation a where
    asPoints :: a -> [(V4 Float, V3 Float)]
    asLines :: a -> [(V4 Float, V3 Float)]
    asTriangles :: a -> [(V4 Float, V3 Float)]

module Tetrahedron
    (Tetrahedron(Tetrahedron),
    trianglesVerticesNormalsAndColours) where

import Graphics.GPipe

data Tetrahedron = Tetrahedron

points :: [V4 Float]
points =
    [V4    0.5     0.0                 0.0                 1, -- bottom
    V4     (-0.5)  0.0                 0.0                 1,
    V4     0.0     0.0                 ((sqrt 3) / (-2.0)) 1,

    V4     0.5     0.0                 0.0                 1, -- right
    V4     0.0     0.0                 ((sqrt 3) / (-2.0)) 1,
    V4     0.0     ((sqrt 3) / 2.0)    (1.0 / (-3.0))      1,

    V4     (-0.5)  0.0                 0.0                 1, -- left
    V4     0.0     ((sqrt 3) / 2.0)    (1.0 / (-3.0))      1,
    V4     0.0     0.0                 ((sqrt 3) / (-2.0)) 1,

    V4     0.5     0.0                 0.0                 1, -- front
    V4     0.0     ((sqrt 3) / 2.0)    (1.0 / (-3.0))      1,
    V4     (-0.5)  0.0                 0.0                 1]
--        [V4 (0.5) (0.5) (0.5) 1, -- top 1
--        V4 (0.5) (0.5) (-0.5) 1,
--        V4 (-0.5) (0.5) (-0.5) 1,

--        V4 (0.5) (0.5) (0.5) 1, -- top 2
--        V4 (-0.5) (0.5) (-0.5) 1,
--        V4 (-0.5) (0.5) (0.5) 1,

--        V4 (0.5) (0.5) (0.5) 1, -- front 1
--        V4 (-0.5) (0.5) (0.5) 1,
--        V4 (-0.5) (-0.5) (0.5) 1,

--        V4 (0.5) (0.5) (0.5) 1, -- front 2
--        V4 (-0.5) (-0.5) (0.5) 1,
--        V4 (0.5) (-0.5) (0.5) 1,

--        V4 (0.5) (0.5) (0.5) 1, -- right 1
--        V4 (0.5) (-0.5) (0.5) 1,
--        V4 (0.5) (-0.5) (-0.5) 1,

--        V4 (0.5) (0.5) (0.5) 1, -- right 2
--        V4 (0.5) (-0.5) (-0.5) 1,
--        V4 (0.5) (0.5) (-0.5) 1,

--        V4 (-0.5) (0.5) (0.5) 1, -- left 1
--        V4 (-0.5) (-0.5) (-0.5) 1,
--        V4 (-0.5) (-0.5) (0.5) 1,

--        V4 (-0.5) (0.5) (0.5) 1, -- left 2
--        V4 (-0.5) (0.5) (-0.5) 1,
--        V4 (-0.5) (-0.5) (-0.5) 1,

--        V4 (0.5) (0.5) (-0.5) 1, -- back 1
--        V4 (0.5) (-0.5) (-0.5) 1,
--        V4 (-0.5) (-0.5) (-0.5) 1,

--        V4 (0.5) (0.5) (-0.5) 1, -- back 2
--        V4 (-0.5) (-0.5) (-0.5) 1,
--        V4 (-0.5) (0.5) (-0.5) 1,

--        V4 (0.5) (-0.5) (0.5) 1, -- bottom 1
--        V4 (-0.5) (-0.5) (0.5) 1,
--        V4 (-0.5) (-0.5) (-0.5) 1,

--        V4 (0.5) (-0.5) (0.5) 1, -- bottom 2
--        V4 (-0.5) (-0.5) (-0.5) 1,
--        V4 (0.5) (-0.5) (-0.5) 1]

lines :: [V4 Float]
lines = getTriangles points
    where
        getTriangles (vertex1 : vertex2 : vertex3 : allOtherVertices) =
            [vertex1, vertex2, vertex2, vertex3, vertex3, vertex1] ++
            getTriangles allOtherVertices
        getTriangles (vertex1 : vertex2 : allOtherVertices) = []
        getTriangles (vertex1 : allOtherVertices) = []
        getTriangles [] = []


triangles :: [V4 Float]
triangles = points

trianglesVerticesNormalsAndColours :: [(V4 Float, V4 Float, V3 Float)]
trianglesVerticesNormalsAndColours = zip3 triangles (calculateNormalVectorsForTriangles triangles) reds

reds :: [V3 Float]
reds = replicate (length triangles) (V3 1.0 0.0 0.0)

calculateNormalVectorsForTriangles1 :: [V4 Float] -> [V4 Float]
calculateNormalVectorsForTriangles1 a = replicate (length triangles) (V4 1.0 1.0 0.0 1.0)

calculateNormalVectorsForPoints :: [V4 Float] -> [V4 Float]
calculateNormalVectorsForPoints ((V4 x0X x0Y x0Z x0W) : (V4 x1X x1Y x1Z x1W) : (V4 x2X x2Y x2Z x2W) : xs) =
    normal1 : normal2 : normal3 : (calculateNormalVectorsForPoints xs)
    where
--        normal1 = V4 1.0 0.5 0.0 1.0
--        normal2 = V4 1.0 0.5 0.0 1.0
--        normal3 = V4 1.0 0.5 0.0 1.0

        normal1 = V4 n1X n1Y n1Z 1.0
        normal2 = V4 n2X n2Y n2Z 1.0
        normal3 = V4 n3X n3Y n3Z 1.0

        n1X = (u1Y * v1Z) - (u1Z * v1Y)
        n1Y = (u1Z * v1X) - (u1X * v1Z)
        n1Z = (u1X * v1Y) - (u1Y * v1X)

        n2X = (u2Y * v2Z) - (u2Z * v2Y)
        n2Y = (u2Z * v2X) - (u2X * v2Z)
        n2Z = (u2X * v2Y) - (u2Y * v2X)

        n3X = (u3Y * v3Z) - (u3Z * v3Y)
        n3Y = (u3Z * v3X) - (u3X * v3Z)
        n3Z = (u3X * v3Y) - (u3Y * v3X)

        -- u1 = p2 - p1 or x1 - x0
        -- v1 = p3 - p1 or x2 - x0

        u1X = x1X - x0X
        u1Y = x1Y - x0Y
        u1Z = x1Z - x0Z
        v1X = x2X - x0X
        v1Y = x2Y - x0Y
        v1Z = x2Z - x0Z

        -- u2 = p3 - p2 or x2 - x1
        -- v2 = p1 - p2 or x0 - x1

        u2X = x2X - x1X
        u2Y = x2Y - x1Y
        u2Z = x2Z - x1Z
        v2X = x0X - x1X
        v2Y = x0Y - x1Y
        v2Z = x0Z - x1Z

        -- u3 = p1 - p3 or x0 - x2
        -- v3 = p2 - p3 or x1 - x2

        u3X = x0X - x2X
        u3Y = x0Y - x2Y
        u3Z = x0Z - x2Z
        v3X = x1X - x2X
        v3Y = x1Y - x2Y
        v3Z = x1Z - x2Z
calculateNormalVectorsForPoints (x0 : x1 : xs) = []
calculateNormalVectorsForPoints (x0 : xs) = []
calculateNormalVectorsForPoints [] = []

calculateNormalVectorsForLines :: [V4 Float] -> [V4 Float]
calculateNormalVectorsForLines vertices = duplicateVertices $ calculateNormalVectorsForPoints $ skimVertices vertices
    where
        duplicateVertices (x : xs) = x : x : duplicateVertices xs
        duplicateVertices [] = []
        skimVertices (x0 : x1 : x2 : x3 : x4 : x5 : xs) = x0 : x2 : x4 : skimVertices xs
        skimVertices (x0 : x1 : x2 : x3 : x4 : xs) = []
        skimVertices (x0 : x1 : x2 : x3 : xs) = []
        skimVertices (x0 : x1 : x2 : xs) = []
        skimVertices (x0 : x1 : xs) = []
        skimVertices (x0 : xs) = []
        skimVertices [] = []

calculateNormalVectorsForTriangles :: [V4 Float] -> [V4 Float]
calculateNormalVectorsForTriangles = calculateNormalVectorsForPoints

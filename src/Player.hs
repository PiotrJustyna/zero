module Player
    (Player(Player),
    PlayerName,
    PlayerHitPoints,
    PlayerLocation,
    zipModelVerticesAndNormalVertices,
    calculateRawNormalsForPoints,
    calculateRawNormalsForLines,
    calculateRawNormalsForTriangles) where

import Representation
import Data.Word
import Graphics.GPipe

type PlayerName = String
type PlayerHitPoints = Word
type PlayerLocation = V3 Float

data Player = Player PlayerName PlayerHitPoints PlayerLocation

instance Show Player where
    show (Player playerName playerHitPoints (V3 x y z)) =
        "Player: " ++ playerName ++
        "\nHit points: " ++ show playerHitPoints ++
        "\nLocation: x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z

instance Projection Player where

    asPoints player =
        [(V4    0.5     0.0                 0.0                 1, red), -- bottom
        (V4     (-0.5)  0.0                 0.0                 1, red),
        (V4     0.0     0.0                 ((sqrt 3) / (-2.0)) 1, red),

        (V4     0.5     0.0                 0.0                 1, red), -- right
        (V4     0.0     0.0                 ((sqrt 3) / (-2.0)) 1, red),
        (V4     0.0     ((sqrt 3) / 2.0)    (1.0 / (-3.0))      1, red),

        (V4     (-0.5)  0.0                 0.0                 1, red), -- left
        (V4     0.0     ((sqrt 3) / 2.0)    (1.0 / (-3.0))      1, red),
        (V4     0.0     0.0                 ((sqrt 3) / (-2.0)) 1, red),

        (V4     0.5     0.0                 0.0                 1, red), -- front
        (V4     0.0     ((sqrt 3) / 2.0)    (1.0 / (-3.0))      1, red),
        (V4     (-0.5)  0.0                 0.0                 1, red)]
        where
            red = V3 1.0 0.0 0.0

    asLines player = getTriangles $ asPoints player
        where
            getTriangles (vertex1 : vertex2 : vertex3 : allOtherVertices) =
                [vertex1, vertex2, vertex2, vertex3, vertex3, vertex1] ++
                getTriangles allOtherVertices
            getTriangles (vertex1 : vertex2 : allOtherVertices) = []
            getTriangles (vertex1 : allOtherVertices) = []
            getTriangles [] = []

    asTriangles = asPoints

    numberOfPointVertices x = length $ asPoints x

    numberOfLineVertices player = length $ asLines player

    numberOfTriangleVertices = numberOfPointVertices

zipModelVerticesAndNormalVertices :: [(V4 Float, V3 Float)]
    -> [(V4 Float, V3 Float)]
    -> [(V4 Float, V3 Float)]
zipModelVerticesAndNormalVertices a b =
    foldl
        (\acc (x, y) -> (x : (y : acc)))
        [] $
        zipWith
            (\(V4 mVX mVY mVZ mVW, mC) (V4 nVX nVY nVZ nVW, nC) ->
                ((V4 mVX mVY mVZ 1.0, nC), (V4 (mVX + nVX) (mVY + nVY) (mVZ + nVZ) 1.0, nC)))
            a
            b

calculateRawNormalsForPoints :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
calculateRawNormalsForPoints ((V4 x0X x0Y x0Z x0W, x0C) : (V4 x1X x1Y x1Z x1W, x1C) : (V4 x2X x2Y x2Z x2W, x2C) : xs) =
    normal1 : normal2 : normal3 : (calculateRawNormalsForPoints xs)
    where
        normal1 = (V4 (n1X / 10.0) (n1Y / 10.0) (n1Z / 10.0) 1.0, white)
        normal2 = (V4 (n2X / 10.0) (n2Y / 10.0) (n2Z / 10.0) 1.0, white)
        normal3 = (V4 (n3X / 10.0) (n3Y / 10.0) (n3Z / 10.0) 1.0, white)

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

        white = V3 1.0 1.0 1.0
calculateRawNormalsForPoints (x0 : x1 : xs) = []
calculateRawNormalsForPoints (x0 : xs) = []
calculateRawNormalsForPoints [] = []

calculateRawNormalsForLines :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
calculateRawNormalsForLines vertices = duplicateVertices $ calculateRawNormalsForPoints $ skimVertices vertices
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

calculateRawNormalsForTriangles :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
calculateRawNormalsForTriangles = calculateRawNormalsForPoints

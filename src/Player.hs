module Player
    (Player(Player),
    PlayerName,
    PlayerHitPoints,
    PlayerLocation,
    hit,
    heal,
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

    numberOfPointVertices x = length $ asPoints x

    asLines (Player playerName playerHitPoints (V3 x y z)) =
        [(V4 (x + 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0), -- bottom
        (V4 (x - 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),
        (V4 (x + 0.0) (y + 0.0) (z - ((sqrt 3) / 2.0)) 1, V3 redChannel 0 0),

        (V4 (x + 0.0) (y + 0.0) (z - ((sqrt 3) / 2.0)) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0), -- right
        (V4 (x + 0.0) (y + 0.0) (z - ((sqrt 3) / 2.0)) 1, V3 redChannel 0 0),

        (V4 (x + 0.0) (y + 0.0) (z - ((sqrt 3) / 2.0)) 1, V3 redChannel 0 0),
        (V4 (x + 0.0) (y + ((sqrt 3) / 2.0)) (z - (1.0 / 3.0)) 1, V3 redChannel 0 0),

        (V4 (x + 0.0) (y + ((sqrt 3) / 2.0)) (z - (1.0 / 3.0)) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0), -- left
        (V4 (x + 0.0) (y + ((sqrt 3) / 2.0)) (z - (1.0 / 3.0)) 1, V3 redChannel 0 0),

        (V4 (x + 0.0) (y + ((sqrt 3) / 2.0)) (z - (1.0 / 3.0)) 1, V3 redChannel 0 0),
        (V4 (x + 0.0) (y + 0.0) (z - ((sqrt 3) / 2.0)) 1, V3 redChannel 0 0),

        (V4 (x + 0.0) (y + 0.0) (z - ((sqrt 3) / 2.0)) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0), -- front
        (V4 (x + 0.0) (y + ((sqrt 3) / 2.0)) (z - (1.0 / 3.0)) 1, V3 redChannel 0 0),

        (V4 (x + 0.0) (y + ((sqrt 3) / 2.0)) (z - (1.0 / 3.0)) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.0) (z + 0.0) 1, V3 redChannel 0 0)]
        where
            greenChannel = fromIntegral playerHitPoints / 100.0
            redChannel = 1.0 - fromIntegral playerHitPoints / 100.0

    numberOfLineVertices x = 24

    asTriangles = asPoints

    numberOfTriangleVertices = numberOfPointVertices

hit :: Player -> Player
hit (Player a b c) = Player a (b - 1) c

heal :: Player -> Player
heal (Player a b c) = Player a (b + 1) c


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

calculateNormalsForSurfaceVectors :: (V3 Float)
    -> (V3 Float)
    -> (V3 Float)
    -> (V3 Float)
    -> (V3 Float)
    -> (V3 Float)
    -> [V4 Float]
calculateNormalsForSurfaceVectors (V3 u1X u1Y u1Z) (V3 u2X u2Y u2Z) (V3 u3X u3Y u3Z) (V3 v1X v1Y v1Z) (V3 v2X v2Y v2Z) (V3 v3X v3Y v3Z) =
    normal1 : normal2 : normal3 : []
    where
        normal1 = V4 (n1X / 10.0) (n1Y / 10.0) (n1Z / 10.0) 1.0
        normal2 = V4 (n2X / 10.0) (n2Y / 10.0) (n2Z / 10.0) 1.0
        normal3 = V4 (n3X / 10.0) (n3Y / 10.0) (n3Z / 10.0) 1.0

        n1X = (u1Y * v1Z) - (u1Z * v1Y)
        n1Y = (u1Z * v1X) - (u1X * v1Z)
        n1Z = (u1X * v1Y) - (u1Y * v1X)

        n2X = (u2Y * v2Z) - (u2Z * v2Y)
        n2Y = (u2Z * v2X) - (u2X * v2Z)
        n2Z = (u2X * v2Y) - (u2Y * v2X)

        n3X = (u3Y * v3Z) - (u3Z * v3Y)
        n3Y = (u3Z * v3X) - (u3X * v3Z)
        n3Z = (u3X * v3Y) - (u3Y * v3X)

calculateRawNormalsForPoints :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
calculateRawNormalsForPoints ((V4 x0X x0Y x0Z x0W, x0C) : (V4 x1X x1Y x1Z x1W, x1C) : (V4 x2X x2Y x2Z x2W, x2C) : xs) =
    (head normals, white) : (normals !! 1, white) : (normals !! 2, white) : (calculateRawNormalsForPoints xs)
    where
        normals = calculateNormalsForSurfaceVectors (V3 u1X u1Y u1Z) (V3 u2X u2Y u2Z) (V3 u3X u3Y u3Z) (V3 v1X v1Y v1Z) (V3 v2X v2Y v2Z) (V3 v3X v3Y v3Z)

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
calculateRawNormalsForLines ((V4 x0X x0Y x0Z x0W, x0C) : x1 : (V4 x2X x2Y x2Z x2W, x2C) : x3 : (V4 x4X x4Y x4Z x4W, x4C) : x5 : xs) =
    (head normals, white) : (normals !! 1, white) : (normals !! 2, white) : (calculateRawNormalsForPoints xs)
    where
        normals = calculateNormalsForSurfaceVectors (V3 u1X u1Y u1Z) (V3 u2X u2Y u2Z) (V3 u3X u3Y u3Z) (V3 v1X v1Y v1Z) (V3 v2X v2Y v2Z) (V3 v3X v3Y v3Z)

        -- u1 = p2 - p1 or x2 - x0
        -- v1 = p3 - p1 or x4 - x0

        u1X = x2X - x0X
        u1Y = x2Y - x0Y
        u1Z = x2Z - x0Z
        v1X = x4X - x0X
        v1Y = x4Y - x0Y
        v1Z = x4Z - x0Z

        -- u2 = p3 - p2 or x4 - x2
        -- v2 = p1 - p2 or x0 - x2

        u2X = x4X - x2X
        u2Y = x4Y - x2Y
        u2Z = x4Z - x2Z
        v2X = x0X - x2X
        v2Y = x0Y - x2Y
        v2Z = x0Z - x2Z

        -- u3 = p1 - p3 or x0 - x4
        -- v3 = p2 - p3 or x2 - x4

        u3X = x0X - x4X
        u3Y = x0Y - x4Y
        u3Z = x0Z - x4Z
        v3X = x2X - x4X
        v3Y = x2Y - x4Y
        v3Z = x2Z - x4Z

        white = V3 1.0 1.0 1.0
calculateRawNormalsForLines (x0 : x1 : x2 : x3 : x4 : xs) = []
calculateRawNormalsForLines (x0 : x1 : x2 : x3 : xs) = []
calculateRawNormalsForLines (x0 : x1 : x2 : xs) = []
calculateRawNormalsForLines (x0 : x1 : xs) = []
calculateRawNormalsForLines (x0 : xs) = []
calculateRawNormalsForLines [] = []

calculateRawNormalsForTriangles :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
calculateRawNormalsForTriangles = calculateRawNormalsForPoints

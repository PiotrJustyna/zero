module Player
    ( Player(Player),
    PlayerName,
    PlayerHitPoints,
    PlayerLocation,
    hit,
    heal,
    zipModelVerticesAndNormalVertices,
    calculateRawNormals) where

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
    asPoints (Player playerName playerHitPoints (V3 x y z)) =
        [(V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- top
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- bottom
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0)]
        where
            greenChannel = fromIntegral playerHitPoints / 100.0
            redChannel = 1.0 - fromIntegral playerHitPoints / 100.0

    numberOfPointVertices x = 8

    asLines (Player playerName playerHitPoints (V3 x y z)) =
        [(V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- top 1
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- top 2
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- front 1
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- front 2
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- right 1
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- right 2
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- left 1
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- left 2
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back 1
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back 2
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- bottom 1
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- bottom 2
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0)]
        where
            greenChannel = fromIntegral playerHitPoints / 100.0
            redChannel = 1.0 - fromIntegral playerHitPoints / 100.0

    numberOfLineVertices x = 72

    asTriangles (Player playerName playerHitPoints (V3 x y z)) =
        [(V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- right
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- front
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- left
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- top
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- bottom
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0)]
        where
            greenChannel = fromIntegral playerHitPoints / 100.0
            redChannel = 1.0 - fromIntegral playerHitPoints / 100.0

    numberOfTriangleVertices x = 36

hit :: Player -> Player
hit (Player a b c) = Player a (b - 1) c

heal :: Player -> Player
heal (Player a b c) = Player a (b + 1) c


zipModelVerticesAndNormalVertices :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
zipModelVerticesAndNormalVertices a b =
    foldl
        (\acc (x, y) -> (x : (y : acc)))
        [] $
        zipWith
            (\(V4 mVX mVY mVZ mVW, mC) (V4 nVX nVY nVZ nVW, nC) ->
                ((V4 mVX mVY mVZ 1.0, nC), (V4 (mVX + nVX) (mVY + nVY) (mVZ + nVZ) 1.0, nC)))
            a
            b

-- "asLines" representation, based on assumption that, even with lines, we draw triangles
-- source: https://www.opengl.org/wiki/Calculating_a_Surface_Normal#Algorithm
calculateRawNormals :: [(V4 Float, V3 Float)] -> [(V4 Float, V3 Float)]
calculateRawNormals ((V4 x0X x0Y x0Z x0W, x0C) : x1 : (V4 x2X x2Y x2Z x2W, x2C) : x3 : (V4 x4X x4Y x4Z x4W, x4C) : x5 : xs) =
    normal1 : normal1 : normal2 : normal2 : normal3 : normal3 : (calculateRawNormals xs)
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
calculateRawNormals (x0 : x1 : x2 : x3 : x4 : xs) = []
calculateRawNormals (x0 : x1 : x2 : x3 : xs) = []
calculateRawNormals (x0 : x1 : x2 : xs) = []
calculateRawNormals (x0 : x1 : xs) = []
calculateRawNormals (x0 : xs) = []
calculateRawNormals [] = []

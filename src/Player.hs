module Player
    ( Player(Player),
    PlayerName,
    PlayerHitPoints,
    PlayerLocation,
    hit,
    heal,
    rawNormals,
    zipModelVerticesAndNormalVertices) where

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
        [(V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- top
        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- bottom
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- sides, front right1
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- front right2
        (V4 (x + 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- front left1
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z + 0.5) 1, V3 redChannel 0 0), -- front left2
        (V4 (x - 0.5) (y - 0.5) (z + 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back left1
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x - 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back left2
        (V4 (x - 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back right1
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0),

        (V4 (x + 0.5) (y + 0.5) (z - 0.5) 1, V3 redChannel 0 0), -- back right2
        (V4 (x + 0.5) (y - 0.5) (z - 0.5) 1, V3 redChannel 0 0)]
        where
            greenChannel = fromIntegral playerHitPoints / 100.0
            redChannel = 1.0 - fromIntegral playerHitPoints / 100.0

    numberOfLineVertices x = 32

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

rawNormals :: [(V4 Float, V3 Float)]
rawNormals =
    [(V4 0.0 0.1 0.0 1.0, white), -- top
    (V4 0.0 0.1 0.0 1.0, white),

    (V4 0.0 0.1 0.0 1.0, white),
    (V4 0.0 0.1 0.0 1.0, white),

    (V4 0.0 0.1 0.0 1.0, white),
    (V4 0.0 0.1 0.0 1.0, white),

    (V4 0.0 0.1 0.0 1.0, white),
    (V4 0.0 0.1 0.0 1.0, white),

    (V4 0.0 (-0.1) 0.0 1.0, white), -- bottom
    (V4 0.0 (-0.1) 0.0 1.0, white),

    (V4 0.0 (-0.1) 0.0 1.0, white),
    (V4 0.0 (-0.1) 0.0 1.0, white),

    (V4 0.0 (-0.1) 0.0 1.0, white),
    (V4 0.0 (-0.1) 0.0 1.0, white),

    (V4 0.0 (-0.1) 0.0 1.0, white),
    (V4 0.0 (-0.1) 0.0 1.0, white),

    (V4 0.0 0.0 0.1 1.0, white), -- sides, front right -> front facing
    (V4 0.0 0.0 0.1 1.0, white),

    (V4 0.1 0.0 0.0 1.0, white), -- front right -> right facing
    (V4 0.1 0.0 0.0 1.0, white),

    (V4 0.0 0.0 0.1 1.0, white), -- front left -> front facing
    (V4 0.0 0.0 0.1 1.0, white),

    (V4 (-0.1) 0.0 0.0 1.0, white), -- front left -> left facing
    (V4 (-0.1) 0.0 0.0 1.0, white),

    (V4 0.0 0.0 (-0.1) 1.0, white), -- back left -> back facing
    (V4 0.0 0.0 (-0.1) 1.0, white),

    (V4 (-0.1) 0.0 0.0 1.0, white), -- back left -> left facing
    (V4 (-0.1) 0.0 0.0 1.0, white),

    (V4 0.0 0.0 (-0.1) 1.0, white), -- back right -> back facing
    (V4 0.0 0.0 (-0.1) 1.0, white),

    (V4 0.1 0.0 0.0 1.0, white), -- back right -> right facing
    (V4 0.1 0.0 0.0 1.0, white)]
    where
        white = V3 1.0 1.0 1.0

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

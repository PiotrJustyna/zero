module Player
    ( Player(Player),
    PlayerName,
    PlayerHitPoints,
    PlayerLocation,
    hit,
    heal) where

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

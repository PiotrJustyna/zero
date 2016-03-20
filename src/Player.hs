module Player
    ( Player(Player),
    PlayerName,
    PlayerHitPoints,
    PlayerLocation,
    getLocation,
    hit,
    heal) where

import Data.Word
import Graphics.GPipe

type PlayerName = String
type PlayerHitPoints = Word
type PlayerLocation = V3 Float

data Player = Player PlayerName PlayerHitPoints PlayerLocation

instance Show Player where
    show (Player playerName playerHitPoints playerLocation) =
        "Player: " ++ playerName ++
        "\nHit points: " ++ show playerHitPoints ++
        "\nLocation: " ++ show playerLocation

getLocation :: Player -> PlayerLocation
getLocation (Player playerName playerHitPoints playerLocation) = playerLocation

hit :: Player -> Player
hit (Player a b c) = Player a (b - 1) c

heal :: Player -> Player
heal (Player a b c) = Player a (b + 1) c

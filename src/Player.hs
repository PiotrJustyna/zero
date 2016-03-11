module Player
    ( Player(Player),
      PlayerName,
      PlayerHitPoints,
      hit,
      heal
    ) where

import Data.Word

type PlayerName = String
type PlayerHitPoints = Word

data Player = Player PlayerName PlayerHitPoints

instance Show Player where
    show (Player playerName playerHitPoints) =
        "Player: " ++ playerName ++
        "\nHit points: " ++ show playerHitPoints

hit :: Player -> Player
hit (Player a b) = Player a (b - 1)

heal :: Player -> Player
heal (Player a b) = Player a (b + 1)

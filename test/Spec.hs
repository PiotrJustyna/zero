import Test.QuickCheck
import Player

main :: IO ()
main = do
    putStrLn "Show Player"
    verboseCheck prop_ShowPlayer

prop_ShowPlayer :: CorrectPlayerNameGenerator -> CorrectPlayerHitPointsGenerator -> Bool
prop_ShowPlayer (CorrectPlayerNameGenerator playerName) (CorrectPlayerHitPointsGenerator playerHitPoints) =
    show (Player playerName playerHitPoints) == "Player: " ++ playerName ++ "\nHit points: " ++ show playerHitPoints

data CorrectPlayerNameGenerator = CorrectPlayerNameGenerator PlayerName deriving Show
instance Arbitrary CorrectPlayerNameGenerator where arbitrary = CorrectPlayerNameGenerator <$> generateCorrectPlayerName

data CorrectPlayerHitPointsGenerator = CorrectPlayerHitPointsGenerator PlayerHitPoints deriving Show
instance Arbitrary CorrectPlayerHitPointsGenerator where arbitrary = CorrectPlayerHitPointsGenerator <$> generateCorrectPlayerHitPoints

generateCorrectPlayerName :: Gen PlayerName
generateCorrectPlayerName = listOf1 generateCorrectCharacter

generateCorrectCharacter :: Gen Char
generateCorrectCharacter = elements ['a' .. 'z']

generateCorrectPlayerHitPoints :: Gen Word
generateCorrectPlayerHitPoints = elements [0 .. 99]

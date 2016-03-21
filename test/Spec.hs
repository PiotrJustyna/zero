import Player
import Control.Monad (liftM3)
import Graphics.GPipe
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "Show Player"
    verboseCheck prop_ShowPlayer

prop_ShowPlayer :: CorrectPlayerNameGenerator -> CorrectPlayerHitPointsGenerator -> CorrectPlayerLocationGenerator -> Bool
prop_ShowPlayer (CorrectPlayerNameGenerator playerName) (CorrectPlayerHitPointsGenerator playerHitPoints) (CorrectPlayerLocationGenerator x y z) =
    show (Player playerName playerHitPoints (V3 x y z)) ==
    "Player: " ++ playerName ++
    "\nHit points: " ++ show playerHitPoints ++
    "\nLocation: x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z

data CorrectPlayerNameGenerator = CorrectPlayerNameGenerator PlayerName deriving Show
instance Arbitrary CorrectPlayerNameGenerator where arbitrary = CorrectPlayerNameGenerator <$> generateCorrectPlayerName

data CorrectPlayerHitPointsGenerator = CorrectPlayerHitPointsGenerator PlayerHitPoints deriving Show
instance Arbitrary CorrectPlayerHitPointsGenerator where arbitrary = CorrectPlayerHitPointsGenerator <$> generateCorrectPlayerHitPoints

data CorrectPlayerLocationGenerator = CorrectPlayerLocationGenerator Float Float Float deriving Show
instance Arbitrary CorrectPlayerLocationGenerator where arbitrary = liftM3 CorrectPlayerLocationGenerator generateCorrectPlayerLocationCoordinate generateCorrectPlayerLocationCoordinate generateCorrectPlayerLocationCoordinate

generateCorrectPlayerName :: Gen PlayerName
generateCorrectPlayerName = listOf1 generateCorrectCharacter

generateCorrectCharacter :: Gen Char
generateCorrectCharacter = elements ['a' .. 'z']

generateCorrectPlayerHitPoints :: Gen Word
generateCorrectPlayerHitPoints = elements [0 .. 99]

generateCorrectPlayerLocationCoordinate :: Gen Float
generateCorrectPlayerLocationCoordinate = elements [(-1.0), (-0.95) .. 1.0]

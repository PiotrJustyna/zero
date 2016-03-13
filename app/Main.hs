module Main where

import Player
import System.IO

main :: IO ()
main = do
    putStrLn "Welcome to zero!"
    putStrLn help
    gameLoop $ Player "Piotr" 30

gameLoop :: Player -> IO ()
gameLoop player = do
    hSetBuffering stdin NoBuffering
    input <- getChar
    case input of
        'h' ->  do
                putStrLn $ "\n" ++ help
                gameLoop player
        'n' ->  do
                putStrLn $ "\n" ++ (show player)
                gameLoop player
        'q' ->  putStrLn "\nBye!"

help =
    "h - help\n" ++
    "n - new game\n" ++
    "q - quit"

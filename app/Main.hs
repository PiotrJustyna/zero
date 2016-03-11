module Main where

import Player

main :: IO ()
main = do
    putStrLn "Welcome to zero!"
    putStrLn "Starting player:"
    putStrLn . show . heal . hit $ Player "Piotr" 30

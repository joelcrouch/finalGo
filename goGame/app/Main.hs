module Main (main) where

import Lib

main :: IO ()
main = gameLoop boardSize (initialGameState boardSize)



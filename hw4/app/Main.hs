module Main where

import Task2 (Point (..), perimeter)

main :: IO ()
main = print $ perimeter $ replicate 10000000 (Point 1 1)

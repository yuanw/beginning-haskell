module Main where

import Lib

main :: IO ()
main = print $ kMeans initializeSimple 2 ([(1,1), (1,2), (4,4), (4, 5)] :: [(Double, Double)]) 0.001

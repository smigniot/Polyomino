module Main where

import Lib
import Data.List (intercalate)

main :: IO ()
main = do
    putStrLn "START"
    putStrLn ("Shapes =\n" ++ (showShapes (buildSortedShapes 5)))
    putStrLn ("Solutions =\n" ++ (intercalate "\n" (map show 
            (buildSolutions 5 10 6))))
            --(buildSolutions 5 12 5))))
    putStrLn "END"

